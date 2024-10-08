/* Copyright 1995-2003,2006,2008-2014,2016-2018,2020,2024
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */

/* #define DEBUGINFO */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>

#include "arrays.h"
#include "async.h"
#include "bdw-gc.h"
#include "deprecation.h"
#include "dynwind.h"
#include "eval.h"
#include "gen-scmconfig.h"
#include "gsubr.h"
#include "hashtab.h"
#include "hooks.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "pairs.h"
#include "ports.h"
#include "simpos.h"
#include "smob.h"
#include "stackchk.h"
#include "stime.h"
#include "strings.h"
#include "struct.h"
#include "symbols.h"
#include "vectors.h"

#ifdef GUILE_DEBUG_MALLOC
#include "debug-malloc.h"
#endif

#include "gc.h"

/* For GC_set_start_callback.  */
#include <gc/gc_mark.h>


/* Size in bytes of the initial heap.  This should be about the size of
   result of 'guile -c "(display (assq-ref (gc-stats)
   'heap-total-allocated))"'.  */

#define DEFAULT_INITIAL_HEAP_SIZE (256 * 1024 * SIZEOF_UINTPTR_T)

/* Set this to != 0 if every cell that is accessed shall be checked:
 */
int scm_debug_cell_accesses_p = 0;
int scm_expensive_debug_cell_accesses_p = 0;

/* Set this to 0 if no additional gc's shall be performed, otherwise set it to
 * the number of cell accesses after which a gc shall be called.
 */
int scm_debug_cells_gc_interval = 0;

/* Hash table that keeps a reference to objects the user wants to protect from
   garbage collection.  */
static SCM scm_protects;




static int needs_gc_after_nonlocal_exit = 0;

/* Arrange to throw an exception on failed allocations.  */
static void*
scm_oom_fn (size_t nbytes)
{
  needs_gc_after_nonlocal_exit = 1;
  scm_report_out_of_memory ();
  return NULL;
}

/* Called within GC -- cannot allocate GC memory.  */
static void
scm_gc_warn_proc (char *fmt, GC_word arg)
{
  /* avoid scm_current_warning_port() b/c the GC lock is already taken
     and the fluid ref might require it */
  fprintf (stderr, fmt, arg);
}

void
scm_gc_after_nonlocal_exit (void)
{
  if (needs_gc_after_nonlocal_exit)
    {
      needs_gc_after_nonlocal_exit = 0;
      GC_gcollect_and_unmap ();
    }
}




/* Hooks.  */
scm_t_c_hook scm_before_gc_c_hook;
scm_t_c_hook scm_before_mark_c_hook;
scm_t_c_hook scm_before_sweep_c_hook;
scm_t_c_hook scm_after_sweep_c_hook;
scm_t_c_hook scm_after_gc_c_hook;


static void
run_before_gc_c_hook (void)
{
  if (!SCM_I_CURRENT_THREAD)
    /* GC while a thread is spinning up; punt.  */
    return;

  scm_c_hook_run (&scm_before_gc_c_hook, NULL);
}


/* GC Statistics Keeping
 */
unsigned long scm_gc_ports_collected = 0;
static long gc_time_taken = 0;
static long gc_start_time = 0;

static unsigned long protected_obj_count = 0;


SCM_SYMBOL (sym_gc_time_taken, "gc-time-taken");
SCM_SYMBOL (sym_heap_size, "heap-size");
SCM_SYMBOL (sym_heap_free_size, "heap-free-size");
SCM_SYMBOL (sym_heap_total_allocated, "heap-total-allocated");
SCM_SYMBOL (sym_heap_allocated_since_gc, "heap-allocated-since-gc");
SCM_SYMBOL (sym_protected_objects, "protected-objects");
SCM_SYMBOL (sym_times, "gc-times");


/* {Scheme Interface to GC}
 */
extern int scm_gc_malloc_yield_percentage;
SCM_DEFINE (scm_gc_stats, "gc-stats", 0, 0, 0,
            (),
	    "Return an association list of statistics about Guile's current\n"
	    "use of storage.\n")
#define FUNC_NAME s_scm_gc_stats
{
  SCM answer;
  GC_word heap_size, free_bytes, unmapped_bytes, bytes_since_gc, total_bytes;
  size_t gc_times;

  GC_get_heap_usage_safe (&heap_size, &free_bytes, &unmapped_bytes,
                          &bytes_since_gc, &total_bytes);
  gc_times = GC_get_gc_no ();

  answer =
    scm_list_n (scm_cons (sym_gc_time_taken, scm_from_long (gc_time_taken)),
		scm_cons (sym_heap_size, scm_from_size_t (heap_size)),
		scm_cons (sym_heap_free_size, scm_from_size_t (free_bytes)),
		scm_cons (sym_heap_total_allocated,
			  scm_from_size_t (total_bytes)),
                scm_cons (sym_heap_allocated_since_gc,
			  scm_from_size_t (bytes_since_gc)),
		scm_cons (sym_protected_objects,
			  scm_from_ulong (protected_obj_count)),
		scm_cons (sym_times, scm_from_size_t (gc_times)),
		SCM_UNDEFINED);

  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc_dump, "gc-dump", 0, 0, 0,
	    (void),
	    "Dump information about the garbage collector's internal data "
	    "structures and memory usage to the standard output.")
#define FUNC_NAME s_scm_gc_dump
{
  GC_dump ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_object_address, "object-address", 1, 0, 0,
            (SCM obj),
	    "Return an integer that for the lifetime of @var{obj} is uniquely\n"
	    "returned by this function for @var{obj}")
#define FUNC_NAME s_scm_object_address
{
  return scm_from_ulong (SCM_UNPACK (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc_disable, "gc-disable", 0, 0, 0,
	    (),
	    "Disables the garbage collector.  Nested calls are permitted.  "
	    "GC is re-enabled once @code{gc-enable} has been called the "
	    "same number of times @code{gc-disable} was called.")
#define FUNC_NAME s_scm_gc_disable
{
  GC_disable ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gc_enable, "gc-enable", 0, 0, 0,
	    (),
	    "Enables the garbage collector.")
#define FUNC_NAME s_scm_gc_enable
{
  GC_enable ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc, "gc", 0, 0, 0,
           (),
	    "Scans all of SCM objects and reclaims for further use those that are\n"
	    "no longer accessible.")
#define FUNC_NAME s_scm_gc
{
  scm_i_gc ("call");
  /* If you're calling scm_gc(), you probably want synchronous
     finalization.  */
  GC_invoke_finalizers ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_i_gc (const char *what)
{
  GC_gcollect ();
}



/* {GC Protection Helper Functions}
 */


/*
 * If within a function you need to protect one or more scheme objects from
 * garbage collection, pass them as parameters to one of the
 * scm_remember_upto_here* functions below.  These functions don't do
 * anything, but since the compiler does not know that they are actually
 * no-ops, it will generate code that calls these functions with the given
 * parameters.  Therefore, you can be sure that the compiler will keep those
 * scheme values alive (on the stack or in a register) up to the point where
 * scm_remember_upto_here* is called.  In other words, place the call to
 * scm_remember_upto_here* _behind_ the last code in your function, that
 * depends on the scheme object to exist.
 *
 * Example: We want to make sure that the string object str does not get
 * garbage collected during the execution of 'some_function' in the code
 * below, because otherwise the characters belonging to str would be freed and
 * 'some_function' might access freed memory.  To make sure that the compiler
 * keeps str alive on the stack or in a register such that it is visible to
 * the conservative gc we add the call to scm_remember_upto_here_1 _after_ the
 * call to 'some_function'.  Note that this would not be necessary if str was
 * used anyway after the call to 'some_function'.
 *   char *chars = scm_i_string_chars (str);
 *   some_function (chars);
 *   scm_remember_upto_here_1 (str);  // str will be alive up to this point.
 */

/* Remove any macro versions of these while defining the functions.
   Functions are always included in the library, for upward binary
   compatibility and in case combinations of GCC and non-GCC are used.  */
#undef scm_remember_upto_here_1
#undef scm_remember_upto_here_2

void
scm_remember_upto_here_1 (SCM obj SCM_UNUSED)
{
  /* Empty.  Protects a single object from garbage collection. */
}

void
scm_remember_upto_here_2 (SCM obj1 SCM_UNUSED, SCM obj2 SCM_UNUSED)
{
  /* Empty.  Protects two objects from garbage collection. */
}

void
scm_remember_upto_here (SCM obj SCM_UNUSED, ...)
{
  /* Empty.  Protects any number of objects from garbage collection. */
}

/*
  These crazy functions prevent garbage collection
  of arguments after the first argument by
  ensuring they remain live throughout the
  function because they are used in the last
  line of the code block.
  It'd be better to have a nice compiler hint to
  aid the conservative stack-scanning GC. --03/09/00 gjb */
SCM
scm_return_first (SCM elt, ...)
{
  return elt;
}

int
scm_return_first_int (int i, ...)
{
  return i;
}


SCM
scm_permanent_object (SCM obj)
{
  return (scm_gc_protect_object (obj));
}


/* Protect OBJ from the garbage collector.  OBJ will not be freed, even if all
   other references are dropped, until the object is unprotected by calling
   scm_gc_unprotect_object (OBJ).  Calls to scm_gc_protect/unprotect_object nest,
   i. e. it is possible to protect the same object several times, but it is
   necessary to unprotect the object the same number of times to actually get
   the object unprotected.  It is an error to unprotect an object more often
   than it has been protected before.  The function scm_protect_object returns
   OBJ.
*/

/* Implementation note:  For every object X, there is a counter which
   scm_gc_protect_object (X) increments and scm_gc_unprotect_object (X) decrements.
*/



static scm_i_pthread_mutex_t gc_protect_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

SCM
scm_gc_protect_object (SCM obj)
{
  SCM handle;

  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&gc_protect_lock);

  handle = scm_hashq_create_handle_x (scm_protects, obj, scm_from_int (0));
  SCM_SETCDR (handle, scm_sum (SCM_CDR (handle), scm_from_int (1)));
  protected_obj_count ++;

  scm_dynwind_end ();

  return obj;
}


/* Remove any protection for OBJ established by a prior call to
   scm_protect_object.  This function returns OBJ.

   See scm_protect_object for more information.  */
SCM
scm_gc_unprotect_object (SCM obj)
{
  SCM handle;

  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&gc_protect_lock);

  handle = scm_hashq_get_handle (scm_protects, obj);
  if (scm_is_false (handle))
    {
      fprintf (stderr, "scm_unprotect_object called on unprotected object\n");
      abort ();
    }
  else
    {
      SCM count = scm_difference (SCM_CDR (handle), scm_from_int (1));
      if (scm_is_eq (count, scm_from_int (0)))
	scm_hashq_remove_x (scm_protects, obj);
      else
	SCM_SETCDR (handle, count);
    }
  protected_obj_count --;

  scm_dynwind_end ();

  return obj;
}

void
scm_gc_register_root (SCM *p)
{
  /* Nothing.  */
}

void
scm_gc_unregister_root (SCM *p)
{
  /* Nothing.  */
}

void
scm_gc_register_roots (SCM *b, unsigned long n)
{
  SCM *p = b;
  for (; p < b + n; ++p)
    scm_gc_register_root (p);
}

void
scm_gc_unregister_roots (SCM *b, unsigned long n)
{
  SCM *p = b;
  for (; p < b + n; ++p)
    scm_gc_unregister_root (p);
}




void
scm_storage_prehistory ()
{
  GC_set_all_interior_pointers (0);
  GC_set_finalize_on_demand (1);

#if (GC_VERSION_MAJOR == 7 && GC_VERSION_MINOR == 4	\
     && GC_VERSION_MICRO == 0)
  /* BDW-GC 7.4.0 has a bug making it loop indefinitely when using more
     than one marker thread: <https://github.com/ivmai/bdwgc/pull/30>.
     Work around it by asking for one marker thread.  */
  setenv ("GC_MARKERS", "1", 1);
#endif

#if SCM_I_GSC_USE_NULL_THREADS
  /* If we have disabled threads in Guile, ensure that the GC doesn't
     spawn any marker threads.  */
  setenv ("GC_MARKERS", "1", 1);
#endif

  GC_INIT ();

  size_t heap_size = GC_get_heap_size ();
  if (heap_size < DEFAULT_INITIAL_HEAP_SIZE)
    GC_expand_hp (DEFAULT_INITIAL_HEAP_SIZE - heap_size);

  /* We only need to register a displacement for those types for which the
     higher bits of the type tag are used to store a pointer (that is, a
     pointer to an 8-octet aligned region).  */
  GC_REGISTER_DISPLACEMENT (scm_tc3_cons);
  GC_REGISTER_DISPLACEMENT (scm_tc3_struct);
  /* GC_REGISTER_DISPLACEMENT (scm_tc3_unused); */

  /* Sanity check.  */
  if (!GC_is_visible (&scm_protects))
    abort ();

  scm_c_hook_init (&scm_before_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_mark_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
}

void
scm_init_gc_protect_object ()
{
  scm_protects = scm_c_make_hash_table (31);

#if 0
  /* We can't have a cleanup handler since we have no thread to run it
     in. */

#ifdef HAVE_ATEXIT
  atexit (cleanup);
#else
#ifdef HAVE_ON_EXIT
  on_exit (cleanup, 0);
#endif
#endif

#endif
}



SCM scm_after_gc_hook;

static SCM after_gc_async_cell;

/* The function after_gc_async_thunk causes the execution of the
 * after-gc-hook.  It is run after the gc, as soon as the asynchronous
 * events are handled by the evaluator.
 */
static SCM
after_gc_async_thunk (void)
{
  /* Fun, no? Hook-run *and* run-hook?  */
  scm_c_hook_run (&scm_after_gc_c_hook, NULL);
  scm_c_run_hook (scm_after_gc_hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}


/* The function queue_after_gc_hook is run by the scm_before_gc_c_hook
 * at the end of the garbage collection.  The only purpose of this
 * function is to mark the after_gc_async (which will eventually lead to
 * the execution of the after_gc_async_thunk).
 */
static void *
queue_after_gc_hook (void * hook_data SCM_UNUSED,
                     void *fn_data SCM_UNUSED,
                     void *data SCM_UNUSED)
{
  scm_thread *t = SCM_I_CURRENT_THREAD;

  if (scm_is_false (SCM_CDR (after_gc_async_cell)))
    {
      SCM_SETCDR (after_gc_async_cell, t->pending_asyncs);
      t->pending_asyncs = after_gc_async_cell;
    }

  return NULL;
}



static void *
start_gc_timer (void * hook_data SCM_UNUSED,
                void *fn_data SCM_UNUSED,
                void *data SCM_UNUSED)
{
  if (!gc_start_time)
    gc_start_time = scm_c_get_internal_run_time ();

  return NULL;
}

static void *
accumulate_gc_timer (void * hook_data SCM_UNUSED,
                void *fn_data SCM_UNUSED,
                void *data SCM_UNUSED)
{
  if (gc_start_time)
    {
      long now = scm_c_get_internal_run_time ();
      gc_time_taken += now - gc_start_time;
      gc_start_time = 0;
    }

  return NULL;
}

static size_t bytes_until_gc = DEFAULT_INITIAL_HEAP_SIZE;
static scm_i_pthread_mutex_t bytes_until_gc_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

void
scm_gc_register_allocation (size_t size)
{
  scm_i_pthread_mutex_lock (&bytes_until_gc_lock);
  if (size > bytes_until_gc)
    {
      bytes_until_gc = GC_get_heap_size ();
      scm_i_pthread_mutex_unlock (&bytes_until_gc_lock);
      GC_gcollect ();
    }
  else
    {
      bytes_until_gc -= size;
      scm_i_pthread_mutex_unlock (&bytes_until_gc_lock);
    }
}



void
scm_init_gc ()
{
  /* `GC_INIT ()' was invoked in `scm_storage_prehistory ()'.  */

  scm_after_gc_hook = scm_make_hook (SCM_INUM0);
  scm_c_define ("after-gc-hook", scm_after_gc_hook);

  /* When the async is to run, the cdr of the gc_async pair gets set to
     the asyncs queue of the current thread.  */
  after_gc_async_cell = scm_cons (scm_c_make_gsubr ("%after-gc-thunk", 0, 0, 0,
                                                    after_gc_async_thunk),
                                  SCM_BOOL_F);

  scm_c_hook_add (&scm_before_gc_c_hook, queue_after_gc_hook, NULL, 0);
  scm_c_hook_add (&scm_before_gc_c_hook, start_gc_timer, NULL, 0);
  scm_c_hook_add (&scm_after_gc_c_hook, accumulate_gc_timer, NULL, 0);

  GC_set_oom_fn (scm_oom_fn);
  GC_set_warn_proc (scm_gc_warn_proc);
  GC_set_start_callback (run_before_gc_c_hook);

#include "gc.x"
}


void
scm_gc_sweep (void)
#define FUNC_NAME "scm_gc_sweep"
{
  /* FIXME */
  fprintf (stderr, "%s: doing nothing\n", FUNC_NAME);
}
#undef FUNC_NAME
