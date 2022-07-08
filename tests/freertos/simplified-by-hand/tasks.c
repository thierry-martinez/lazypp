/*
 * FreeRTOS Kernel V10.3.1
 * Copyright (C) 2020 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * http://www.FreeRTOS.org
 * http://aws.amazon.com/freertos
 *
 * 1 tab == 4 spaces!
 */

/* Standard includes. */
#include <stdlib.h>
#include <string.h>

/* Defining MPU_WRAPPERS_INCLUDED_FROM_API_FILE prevents task.h from redefining
   all the API functions to use the MPU wrappers.  That should only be done when
   task.h is included from an application file. */
#define MPU_WRAPPERS_INCLUDED_FROM_API_FILE

/* FreeRTOS includes. */
#include "FreeRTOS.h"
#include "task.h"
#include "timers.h"
#include "stack_macros.h"

#undef MPU_WRAPPERS_INCLUDED_FROM_API_FILE

/* If the cooperative scheduler is being used then a yield should not be
   performed just because a higher priority task has been woken. */
#define taskYIELD_IF_USING_PREEMPTION()

/* Values that can be assigned to the ucNotifyState member of the TCB. */
#define taskNOT_WAITING_NOTIFICATION	( ( uint8_t ) 0 )
#define taskWAITING_NOTIFICATION		( ( uint8_t ) 1 )
#define taskNOTIFICATION_RECEIVED		( ( uint8_t ) 2 )

/*
 * The value used to fill the stack of a task when the task is created.  This
 * is used purely for checking the high water mark for tasks.
 */
#define tskSTACK_FILL_BYTE	( 0xa5U )

/* Bits used to recored how a task's stack and TCB were allocated. */
#define tskDYNAMICALLY_ALLOCATED_STACK_AND_TCB 		( ( uint8_t ) 0 )
#define tskSTATICALLY_ALLOCATED_STACK_ONLY 			( ( uint8_t ) 1 )
#define tskSTATICALLY_ALLOCATED_STACK_AND_TCB		( ( uint8_t ) 2 )

/*
 * Macros used by vListTask to indicate which state a task is in.
 */
#define tskRUNNING_CHAR		( 'X' )
#define tskBLOCKED_CHAR		( 'B' )
#define tskREADY_CHAR		( 'R' )
#define tskDELETED_CHAR		( 'D' )
#define tskSUSPENDED_CHAR	( 'S' )

/* The name allocated to the Idle task.  This can be overridden by defining
   configIDLE_TASK_NAME in FreeRTOSConfig.h. */
#define configIDLE_TASK_NAME "IDLE"

/* If configUSE_PORT_OPTIMISED_TASK_SELECTION is 0 then task selection is
   performed in a generic way that is not optimised to any particular
   microcontroller architecture. */

/* uxTopReadyPriority holds the priority of the highest priority ready
   state task. */
#define taskRECORD_READY_PRIORITY( uxPriority ) \
  {                                             \
   if( ( uxPriority ) > uxTopReadyPriority )    \
     {                                          \
      uxTopReadyPriority = ( uxPriority );      \
     }                                          \
  } /* taskRECORD_READY_PRIORITY */

/*-----------------------------------------------------------*/

#define taskSELECT_HIGHEST_PRIORITY_TASK()                              \
  {                                                                     \
    UBaseType_t uxTopPriority = uxTopReadyPriority;                     \
    /* Find the highest priority queue that contains ready tasks. */    \
    while( listLIST_IS_EMPTY( &( pxReadyTasksLists[ uxTopPriority ] ) ) ) \
      {                                                                 \
        configASSERT( uxTopPriority );                                  \
        --uxTopPriority;                                                \
      }                                                                 \
    /* listGET_OWNER_OF_NEXT_ENTRY indexes through the list, so the tasks of \
       the	same priority get an equal share of the processor time. */ \
    listGET_OWNER_OF_NEXT_ENTRY( pxCurrentTCB, &( pxReadyTasksLists[ uxTopPriority ] ) ); \
    uxTopReadyPriority = uxTopPriority;                                 \
  } /* taskSELECT_HIGHEST_PRIORITY_TASK */

/*-----------------------------------------------------------*/

/* Define away taskRESET_READY_PRIORITY() and portRESET_READY_PRIORITY() as
   they are only required when a port optimised method of task selection is
   being used. */
#define taskRESET_READY_PRIORITY( uxPriority )
#define portRESET_READY_PRIORITY( uxPriority, uxTopReadyPriority )

/*-----------------------------------------------------------*/

/* pxDelayedTaskList and pxOverflowDelayedTaskList are switched when the tick
   count overflows. */
#define taskSWITCH_DELAYED_LISTS()                                      \
  {                                                                     \
    List_t *pxTemp;                                                     \
    /* The delayed tasks list should be empty when the lists are switched. */ \
    configASSERT( ( listLIST_IS_EMPTY( pxDelayedTaskList ) ) );         \
    pxTemp = pxDelayedTaskList;                                         \
    pxDelayedTaskList = pxOverflowDelayedTaskList;                      \
    pxOverflowDelayedTaskList = pxTemp;                                 \
    xNumOfOverflows++;                                                  \
    prvResetNextTaskUnblockTime();                                      \
  }

/*-----------------------------------------------------------*/

/*
 * Place the task represented by pxTCB into the appropriate ready list for
 * the task.  It is inserted at the end of the list.
 */
#define prvAddTaskToReadyList( pxTCB )                                  \
  traceMOVED_TASK_TO_READY_STATE( pxTCB );                              \
  taskRECORD_READY_PRIORITY( ( pxTCB )->uxPriority );                   \
  vListInsertEnd( &( pxReadyTasksLists[ ( pxTCB )->uxPriority ] ), &( ( pxTCB )->xStateListItem ) ); \
  tracePOST_MOVED_TASK_TO_READY_STATE( pxTCB )
/*-----------------------------------------------------------*/

/*
 * Several functions take an TaskHandle_t parameter that can optionally be NULL,
 * where NULL is used to indicate that the handle of the currently executing
 * task should be used in place of the parameter.  This macro simply checks to
 * see if the parameter is NULL and returns a pointer to the appropriate TCB.
 */
#define prvGetTCBFromHandle( pxHandle ) ( ( ( pxHandle ) == NULL ) ? pxCurrentTCB : ( pxHandle ) )

/* The item value of the event list item is normally used to hold the priority
   of the task to which it belongs (coded to allow it to be held in reverse
   priority order).  However, it is occasionally borrowed for other purposes.  It
   is important its value is not updated due to a task priority change while it is
   being used for another purpose.  The following bit definition is used to inform
   the scheduler that the value should not be changed - in which case it is the
   responsibility of whichever module is using the value to ensure it gets set back
   to its original value when it is released. */
#define taskEVENT_LIST_ITEM_VALUE_IN_USE	0x80000000UL

/*
 * Task control block.  A task control block (TCB) is allocated for each task,
 * and stores task state information, including a pointer to the task's context
 * (the task's run time environment, including register values)
 */
typedef struct tskTaskControlBlock 			/* The old naming convention is used to prevent breaking kernel aware debuggers. */
{
  volatile StackType_t *pxTopOfStack;	      /*< Points to the location of the last item placed on the tasks stack.  THIS MUST BE THE FIRST MEMBER OF THE TCB STRUCT. */
  xMPU_SETTINGS	xMPUSettings;	              /*< The MPU settings are defined as part of the port layer.  THIS MUST BE THE SECOND MEMBER OF THE TCB STRUCT. */
  ListItem_t xStateListItem;                  /*< The list that the state list item of a task is reference from denotes the state of that task (Ready, Blocked, Suspended ). */
  ListItem_t xEventListItem;	              /*< Used to reference a task from an event list. */
  UBaseType_t uxPriority;	              /*< The priority of the task.  0 is the lowest priority. */
  StackType_t *pxStack;			      /*< Points to the start of the stack. */
  char pcTaskName[ configMAX_TASK_NAME_LEN ]; /*< Descriptive name given to the task when created.  Facilitates debugging only. */
  uint8_t ucStaticallyAllocated; 	      /*< Set to pdTRUE if the task is a statically allocated to ensure no attempt is made to free the memory. */
  int iTaskErrno;
} tskTCB;

/* The old tskTCB name is maintained above then typedefed to the new TCB_t name
   below to enable the use of older kernel aware debuggers. */
typedef tskTCB TCB_t;

PRIVILEGED_DATA TCB_t * volatile pxCurrentTCB = NULL;

/* Lists for ready and blocked tasks. --------------------
   xDelayedTaskList1 and xDelayedTaskList2 could be move to function scople but
   doing so breaks some kernel aware debuggers and debuggers that rely on removing
   the static qualifier. */
PRIVILEGED_DATA static List_t pxReadyTasksLists[ configMAX_PRIORITIES ];/*< Prioritised ready tasks. */
PRIVILEGED_DATA static List_t xDelayedTaskList1;			/*< Delayed tasks. */
PRIVILEGED_DATA static List_t xDelayedTaskList2;       			/*< Delayed tasks (two lists are used - one for delays
                                                                         *  that have overflowed the current tick count. */
PRIVILEGED_DATA static List_t * volatile pxDelayedTaskList;	       	/*< Points to the delayed task list currently being used. */
PRIVILEGED_DATA static List_t * volatile pxOverflowDelayedTaskList;    	/*< Points to the delayed task list currently being used
                                                                         *  to hold tasks that have overflowed the current tick count. */
PRIVILEGED_DATA static List_t xPendingReadyList;                        /*< Tasks that have been readied while the scheduler was suspended.
                                                                         *  They will be moved to the ready list when the scheduler is resumed. */
PRIVILEGED_DATA static List_t xTasksWaitingTermination;		        /*< Tasks that have been deleted - but their memory not yet freed. */
PRIVILEGED_DATA static volatile UBaseType_t uxDeletedTasksWaitingCleanUp = ( UBaseType_t ) 0U;
PRIVILEGED_DATA static List_t xSuspendedTaskList;			/*< Tasks that are currently suspended. */

/* Global POSIX errno. Its value is changed upon context switching to match
   the errno of the currently running task. */
int FreeRTOS_errno = 0;

/* Other file private variables. --------------------------------*/
PRIVILEGED_DATA static volatile UBaseType_t uxCurrentNumberOfTasks 	= ( UBaseType_t ) 0U;
PRIVILEGED_DATA static volatile TickType_t xTickCount 			= ( TickType_t ) configINITIAL_TICK_COUNT;
PRIVILEGED_DATA static volatile UBaseType_t uxTopReadyPriority 		= tskIDLE_PRIORITY;
PRIVILEGED_DATA static volatile BaseType_t xSchedulerRunning 		= pdFALSE;
PRIVILEGED_DATA static volatile TickType_t xPendedTicks 		= ( TickType_t ) 0U;
PRIVILEGED_DATA static volatile BaseType_t xYieldPending 		= pdFALSE;
PRIVILEGED_DATA static volatile BaseType_t xNumOfOverflows 		= ( BaseType_t ) 0;
PRIVILEGED_DATA static UBaseType_t uxTaskNumber 			= ( UBaseType_t ) 0U;
PRIVILEGED_DATA static volatile TickType_t xNextTaskUnblockTime		= ( TickType_t ) 0U; /* Initialised to portMAX_DELAY before the scheduler starts. */
PRIVILEGED_DATA static TaskHandle_t xIdleTaskHandle			= NULL;			/*< Holds the handle of the idle task.  The idle task is created automatically when the scheduler is started. */

/* Context switches are held pending while the scheduler is suspended.  Also,
   interrupts must not manipulate the xStateListItem of a TCB, or any of the
   lists the xStateListItem can be referenced from, if the scheduler is suspended.
   If an interrupt needs to unblock a task while the scheduler is suspended then it
   moves the task's event list item into the xPendingReadyList, ready for the
   kernel to move the task from the pending ready list into the real ready list
   when the scheduler is unsuspended.  The pending ready list itself can only be
   accessed from a critical section. */
PRIVILEGED_DATA static volatile UBaseType_t uxSchedulerSuspended	= ( UBaseType_t ) pdFALSE;

/*-----------------------------------------------------------*/

/* Callback function prototypes. --------------------------*/

extern void vApplicationGetIdleTaskMemory( StaticTask_t **ppxIdleTaskTCBBuffer, StackType_t **ppxIdleTaskStackBuffer, uint32_t *pulIdleTaskStackSize );

/* File private functions. --------------------------------*/

/*
 * Utility task that simply returns pdTRUE if the task referenced by xTask is
 * currently in the Suspended state, or pdFALSE if the task referenced by xTask
 * is in any other state.
 */
static BaseType_t prvTaskIsTaskSuspended( const TaskHandle_t xTask ) PRIVILEGED_FUNCTION;

/*
 * Utility to ready all the lists used by the scheduler.  This is called
 * automatically upon the creation of the first task.
 */
static void prvInitialiseTaskLists( void ) PRIVILEGED_FUNCTION;

/*
 * The idle task, which as all tasks is implemented as a never ending loop.
 * The idle task is automatically created and added to the ready lists upon
 * creation of the first user task.
 *
 * The portTASK_FUNCTION_PROTO() macro is used to allow port/compiler specific
 * language extensions.  The equivalent prototype for this function is:
 *
 * void prvIdleTask( void *pvParameters );
 *
 */
static portTASK_FUNCTION_PROTO( prvIdleTask, pvParameters );

/*
 * Utility to free all memory allocated by the scheduler to hold a TCB,
 * including the stack pointed to by the TCB.
 *
 * This does not free memory allocated by the task itself (i.e. memory
 * allocated by calls to pvPortMalloc from within the tasks application code).
 */
static void prvDeleteTCB( TCB_t *pxTCB ) PRIVILEGED_FUNCTION;

/*
 * Used only by the idle task.  This checks to see if anything has been placed
 * in the list of tasks waiting to be deleted.  If so the task is cleaned up
 * and its TCB deleted.
 */
static void prvCheckTasksWaitingTermination( void ) PRIVILEGED_FUNCTION;

/*
 * The currently executing task is entering the Blocked state.  Add the task to
 * either the current or the overflow delayed task list.
 */
static void prvAddCurrentTaskToDelayedList( TickType_t xTicksToWait, const BaseType_t xCanBlockIndefinitely ) PRIVILEGED_FUNCTION;

/*
 * Searches pxList for a task with name pcNameToQuery - returning a handle to
 * the task if it is found, or NULL if the task is not found.
 */
static TCB_t *prvSearchForNameWithinSingleList( List_t *pxList, const char pcNameToQuery[] ) PRIVILEGED_FUNCTION;

/*
 * Set xNextTaskUnblockTime to the time at which the next Blocked state task
 * will exit the Blocked state.
 */
static void prvResetNextTaskUnblockTime( void );

/*
 * Called after a Task_t structure has been allocated either statically or
 * dynamically to fill in the structure's members.
 */
static void prvInitialiseNewTask( 	TaskFunction_t pxTaskCode,
                                        const char * const pcName,
                                        const uint32_t ulStackDepth,
                                        void * const pvParameters,
                                        UBaseType_t uxPriority,
                                        TaskHandle_t * const pxCreatedTask,
                                        TCB_t *pxNewTCB,
                                        const MemoryRegion_t * const xRegions ) PRIVILEGED_FUNCTION;

/*
 * Called after a new task has been created and initialised to place the task
 * under the control of the scheduler.
 */
static void prvAddNewTaskToReadyList( TCB_t *pxNewTCB ) PRIVILEGED_FUNCTION;

/*-----------------------------------------------------------*/

TaskHandle_t xTaskCreateStatic(	TaskFunction_t pxTaskCode,
                                const char * const pcName,
                                const uint32_t ulStackDepth,
                                void * const pvParameters,
                                UBaseType_t uxPriority,
                                StackType_t * const puxStackBuffer,
                                StaticTask_t * const pxTaskBuffer )
{
  TCB_t *pxNewTCB;
  TaskHandle_t xReturn;
  configASSERT( puxStackBuffer != NULL );
  configASSERT( pxTaskBuffer != NULL );
  /* Sanity check that the size of the structure used to declare a
     variable of type StaticTask_t equals the size of the real task
     structure. */
  volatile size_t xSize = sizeof( StaticTask_t );
  configASSERT( xSize == sizeof( TCB_t ) );
  ( void ) xSize;
  if( ( pxTaskBuffer != NULL ) && ( puxStackBuffer != NULL ) )
    {
      /* The memory used for the task's TCB and stack are passed into this
         function - use them. */
      pxNewTCB = ( TCB_t * ) pxTaskBuffer;
      pxNewTCB->pxStack = ( StackType_t * ) puxStackBuffer;
      /* Tasks can be created statically or dynamically, so note this
         task was created statically in case the task is later deleted. */
      pxNewTCB->ucStaticallyAllocated = tskSTATICALLY_ALLOCATED_STACK_AND_TCB;
      prvInitialiseNewTask( pxTaskCode, pcName, ulStackDepth, pvParameters, uxPriority, &xReturn, pxNewTCB, NULL );
      prvAddNewTaskToReadyList( pxNewTCB );
    }
  else
    xReturn = NULL;
  return xReturn;
}

BaseType_t xTaskCreateRestrictedStatic( const TaskParameters_t * const pxTaskDefinition, TaskHandle_t *pxCreatedTask )
{
  TCB_t *pxNewTCB;
  BaseType_t xReturn = errCOULD_NOT_ALLOCATE_REQUIRED_MEMORY;
  configASSERT( pxTaskDefinition->puxStackBuffer != NULL );
  configASSERT( pxTaskDefinition->pxTaskBuffer != NULL );
  if( ( pxTaskDefinition->puxStackBuffer != NULL ) && ( pxTaskDefinition->pxTaskBuffer != NULL ) )
    {
      /* Allocate space for the TCB.  Where the memory comes from depends
         on the implementation of the port malloc function and whether or
         not static allocation is being used. */
      pxNewTCB = ( TCB_t * ) pxTaskDefinition->pxTaskBuffer;
      /* Store the stack location in the TCB. */
      pxNewTCB->pxStack = pxTaskDefinition->puxStackBuffer;
      /* Tasks can be created statically or dynamically, so note this
         task was created statically in case the task is later deleted. */
      pxNewTCB->ucStaticallyAllocated = tskSTATICALLY_ALLOCATED_STACK_AND_TCB;
      prvInitialiseNewTask(	pxTaskDefinition->pvTaskCode,
                                pxTaskDefinition->pcName,
                                ( uint32_t ) pxTaskDefinition->usStackDepth,
                                pxTaskDefinition->pvParameters,
                                pxTaskDefinition->uxPriority,
                                pxCreatedTask, pxNewTCB,
                                pxTaskDefinition->xRegions );
      prvAddNewTaskToReadyList( pxNewTCB );
      xReturn = pdPASS;
    }
  return xReturn;
}
/*-----------------------------------------------------------*/

BaseType_t xTaskCreateRestricted( const TaskParameters_t * const pxTaskDefinition, TaskHandle_t *pxCreatedTask )
{
  TCB_t *pxNewTCB;
  BaseType_t xReturn = errCOULD_NOT_ALLOCATE_REQUIRED_MEMORY;
  configASSERT( pxTaskDefinition->puxStackBuffer );
  if( pxTaskDefinition->puxStackBuffer != NULL )
    {
      /* Allocate space for the TCB.  Where the memory comes from depends
         on the implementation of the port malloc function and whether or
         not static allocation is being used. */
      pxNewTCB = ( TCB_t * ) pvPortMalloc( sizeof( TCB_t ) );
      if( pxNewTCB != NULL )
        {
          /* Store the stack location in the TCB. */
          pxNewTCB->pxStack = pxTaskDefinition->puxStackBuffer;
          /* Tasks can be created statically or dynamically, so note
             this task had a statically allocated stack in case it is
             later deleted.  The TCB was allocated dynamically. */
          pxNewTCB->ucStaticallyAllocated = tskSTATICALLY_ALLOCATED_STACK_ONLY;
          prvInitialiseNewTask(	pxTaskDefinition->pvTaskCode,
                                pxTaskDefinition->pcName,
                                ( uint32_t ) pxTaskDefinition->usStackDepth,
                                pxTaskDefinition->pvParameters,
                                pxTaskDefinition->uxPriority,
                                pxCreatedTask, pxNewTCB,
                                pxTaskDefinition->xRegions );
          prvAddNewTaskToReadyList( pxNewTCB );
          xReturn = pdPASS;
        }
    }
  return xReturn;
}

/*-----------------------------------------------------------*/

BaseType_t xTaskCreate(	TaskFunction_t pxTaskCode,
                        const char * const pcName,
                        const configSTACK_DEPTH_TYPE usStackDepth,
                        void * const pvParameters,
                        UBaseType_t uxPriority,
                        TaskHandle_t * const pxCreatedTask )
{
  TCB_t *pxNewTCB;
  BaseType_t xReturn;
  /* If the stack grows down then allocate the stack then the TCB so the stack
     does not grow into the TCB.  Likewise if the stack grows up then allocate
     the TCB then the stack. */
  StackType_t *pxStack;
  /* Allocate space for the stack used by the task being created. */
  pxStack = pvPortMalloc( ( ( ( size_t ) usStackDepth ) * sizeof( StackType_t ) ) );
  if( pxStack != NULL )
    {
      /* Allocate space for the TCB. */
      pxNewTCB = ( TCB_t * ) pvPortMalloc( sizeof( TCB_t ) );
      if( pxNewTCB != NULL )
        /* Store the stack location in the TCB. */
        pxNewTCB->pxStack = pxStack;
      else
        /* The stack cannot be used as the TCB was not created.  Free
           it again. */
        vPortFree( pxStack );
    }
  else
    pxNewTCB = NULL;
  if( pxNewTCB != NULL )
    {
      /* Tasks can be created statically or dynamically, so note this
         task was created dynamically in case it is later deleted. */
      pxNewTCB->ucStaticallyAllocated = tskDYNAMICALLY_ALLOCATED_STACK_AND_TCB;
      prvInitialiseNewTask( pxTaskCode, pcName, ( uint32_t ) usStackDepth, pvParameters, uxPriority, pxCreatedTask, pxNewTCB, NULL );
      prvAddNewTaskToReadyList( pxNewTCB );
      xReturn = pdPASS;
    }
  else
    xReturn = errCOULD_NOT_ALLOCATE_REQUIRED_MEMORY;
  return xReturn;
}
/*-----------------------------------------------------------*/

static void prvInitialiseNewTask( 	TaskFunction_t pxTaskCode,
                                        const char * const pcName,
                                        const uint32_t ulStackDepth,
                                        void * const pvParameters,
                                        UBaseType_t uxPriority,
                                        TaskHandle_t * const pxCreatedTask,
                                        TCB_t *pxNewTCB,
                                        const MemoryRegion_t * const xRegions )
{
  StackType_t *pxTopOfStack;
  UBaseType_t x;
  /* Should the task be created in privileged mode? */
  BaseType_t xRunPrivileged;
  if( ( uxPriority & portPRIVILEGE_BIT ) != 0U )
    xRunPrivileged = pdTRUE;
  else
    xRunPrivileged = pdFALSE;
  uxPriority &= ~portPRIVILEGE_BIT;
  /* Calculate the top of stack address.  This depends on whether the stack
     grows from high memory to low (as per the 80x86) or vice versa.
     portSTACK_GROWTH is used to make the result positive or negative as required
     by the port. */
  pxTopOfStack = &( pxNewTCB->pxStack[ ulStackDepth - ( uint32_t ) 1 ] );
  pxTopOfStack = ( StackType_t * ) ( ( ( portPOINTER_SIZE_TYPE ) pxTopOfStack ) & ( ~( ( portPOINTER_SIZE_TYPE ) portBYTE_ALIGNMENT_MASK ) ) );
  /* Check the alignment of the calculated top of stack is correct. */
  configASSERT( ( ( ( portPOINTER_SIZE_TYPE ) pxTopOfStack & ( portPOINTER_SIZE_TYPE ) portBYTE_ALIGNMENT_MASK ) == 0UL ) );
  /* Store the task name in the TCB. */
  if( pcName != NULL )
    {
      for( x = ( UBaseType_t ) 0; x < ( UBaseType_t ) configMAX_TASK_NAME_LEN; x++ )
        {
          pxNewTCB->pcTaskName[ x ] = pcName[ x ];
          /* Don't copy all configMAX_TASK_NAME_LEN if the string is shorter than
             configMAX_TASK_NAME_LEN characters just in case the memory after the
             string is not accessible (extremely unlikely). */
          if( pcName[ x ] == ( char ) 0x00 )
            break;
          else
            mtCOVERAGE_TEST_MARKER();
        }
      /* Ensure the name string is terminated in the case that the string length
         was greater or equal to configMAX_TASK_NAME_LEN. */
      pxNewTCB->pcTaskName[ configMAX_TASK_NAME_LEN - 1 ] = '\0';
    }
  else
    /* The task has not been given a name, so just ensure there is a NULL
       terminator when it is read out. */
    pxNewTCB->pcTaskName[ 0 ] = 0x00;
  /* This is used as an array index so must ensure it's not too large.  First
     remove the privilege bit if one is present. */
  if( uxPriority >= ( UBaseType_t ) configMAX_PRIORITIES )
    uxPriority = ( UBaseType_t ) configMAX_PRIORITIES - ( UBaseType_t ) 1U;
  else
    mtCOVERAGE_TEST_MARKER();
  pxNewTCB->uxPriority = uxPriority;
  vListInitialiseItem( &( pxNewTCB->xStateListItem ) );
  vListInitialiseItem( &( pxNewTCB->xEventListItem ) );
  /* Set the pxNewTCB as a link back from the ListItem_t.  This is so we can get
     back to	the containing TCB from a generic item in a list. */
  listSET_LIST_ITEM_OWNER( &( pxNewTCB->xStateListItem ), pxNewTCB );
  /* Event lists are always in priority order. */
  listSET_LIST_ITEM_VALUE( &( pxNewTCB->xEventListItem ), ( TickType_t ) configMAX_PRIORITIES - ( TickType_t ) uxPriority );
  listSET_LIST_ITEM_OWNER( &( pxNewTCB->xEventListItem ), pxNewTCB );
  vPortStoreTaskMPUSettings( &( pxNewTCB->xMPUSettings ), xRegions, pxNewTCB->pxStack, ulStackDepth );
  /* Initialize the TCB stack to look as if the task was already running,
     but had been interrupted by the scheduler.  The return address is set
     to the start of the task function. Once the stack has been initialised
     the top of stack variable is updated. */
  /* If the port has capability to detect stack overflow,
     pass the stack end address to the stack initialization
     function as well. */
  pxNewTCB->pxTopOfStack = pxPortInitialiseStack( pxTopOfStack, pxTaskCode, pvParameters, xRunPrivileged );
  if( pxCreatedTask != NULL )
    /* Pass the handle out in an anonymous way.  The handle can be used to
       change the created task's priority, delete the created task, etc.*/
    *pxCreatedTask = ( TaskHandle_t ) pxNewTCB;
  else
    mtCOVERAGE_TEST_MARKER();
}
/*-----------------------------------------------------------*/

static void prvAddNewTaskToReadyList( TCB_t *pxNewTCB )
{
  /* Ensure interrupts don't access the task lists while the lists are being
     updated. */
  taskENTER_CRITICAL();
  uxCurrentNumberOfTasks++;
  if( pxCurrentTCB == NULL )
    {
      /* There are no other tasks, or all the other tasks are in
         the suspended state - make this the current task. */
      pxCurrentTCB = pxNewTCB;
      if( uxCurrentNumberOfTasks == ( UBaseType_t ) 1 )
        /* This is the first task to be created so do the preliminary
           initialisation required.  We will not recover if this call
           fails, but we will report the failure. */
        prvInitialiseTaskLists();
      else
        mtCOVERAGE_TEST_MARKER();
    }
  else
    /* If the scheduler is not already running, make this task the
       current task if it is the highest priority task to be created
       so far. */
    if( xSchedulerRunning == pdFALSE )
      if( pxCurrentTCB->uxPriority <= pxNewTCB->uxPriority )
        pxCurrentTCB = pxNewTCB;
      else
        mtCOVERAGE_TEST_MARKER();
    else
      mtCOVERAGE_TEST_MARKER();
  uxTaskNumber++;
  traceTASK_CREATE( pxNewTCB );
  prvAddTaskToReadyList( pxNewTCB );
  portSETUP_TCB( pxNewTCB );
  taskEXIT_CRITICAL();
  if( xSchedulerRunning != pdFALSE )
    /* If the created task is of a higher priority than the current task
       then it should run now. */
    if( pxCurrentTCB->uxPriority < pxNewTCB->uxPriority )
      taskYIELD_IF_USING_PREEMPTION();
    else
      mtCOVERAGE_TEST_MARKER();
  else
    mtCOVERAGE_TEST_MARKER();
}

void vTaskDelete( TaskHandle_t xTaskToDelete )
{
  TCB_t *pxTCB;
  taskENTER_CRITICAL();
  /* If null is passed in here then it is the calling task that is
     being deleted. */
  pxTCB = prvGetTCBFromHandle( xTaskToDelete );
  /* Remove task from the ready/delayed list. */
  if( uxListRemove( &( pxTCB->xStateListItem ) ) == ( UBaseType_t ) 0 )
    taskRESET_READY_PRIORITY( pxTCB->uxPriority );
  else
    mtCOVERAGE_TEST_MARKER();
  /* Is the task waiting on an event also? */
  if( listLIST_ITEM_CONTAINER( &( pxTCB->xEventListItem ) ) != NULL )
    ( void ) uxListRemove( &( pxTCB->xEventListItem ) );
  else
    mtCOVERAGE_TEST_MARKER();
  /* Increment the uxTaskNumber also so kernel aware debuggers can
     detect that the task lists need re-generating.  This is done before
     portPRE_TASK_DELETE_HOOK() as in the Windows port that macro will
     not return. */
  uxTaskNumber++;
  if( pxTCB == pxCurrentTCB )
    {
      /* A task is deleting itself.  This cannot complete within the
         task itself, as a context switch to another task is required.
         Place the task in the termination list.  The idle task will
         check the termination list and free up any memory allocated by
         the scheduler for the TCB and stack of the deleted task. */
      vListInsertEnd( &xTasksWaitingTermination, &( pxTCB->xStateListItem ) );
      /* Increment the ucTasksDeleted variable so the idle task knows
         there is a task that has been deleted and that it should therefore
         check the xTasksWaitingTermination list. */
      ++uxDeletedTasksWaitingCleanUp;
      /* Call the delete hook before portPRE_TASK_DELETE_HOOK() as
         portPRE_TASK_DELETE_HOOK() does not return in the Win32 port. */
      traceTASK_DELETE( pxTCB );
      /* The pre-delete hook is primarily for the Windows simulator,
         in which Windows specific clean up operations are performed,
         after which it is not possible to yield away from this task -
         hence xYieldPending is used to latch that a context switch is
         required. */
      portPRE_TASK_DELETE_HOOK( pxTCB, &xYieldPending );
    }
  else
    {
      --uxCurrentNumberOfTasks;
      traceTASK_DELETE( pxTCB );
      prvDeleteTCB( pxTCB );
      /* Reset the next expected unblock time in case it referred to
         the task that has just been deleted. */
      prvResetNextTaskUnblockTime();
    }
  taskEXIT_CRITICAL();
  /* Force a reschedule if it is the currently running task that has just
     been deleted. */
  if( xSchedulerRunning != pdFALSE )
    if( pxTCB == pxCurrentTCB )
      {
        configASSERT( uxSchedulerSuspended == 0 );
        portYIELD_WITHIN_API();
      }
    else
      mtCOVERAGE_TEST_MARKER();
}

void vTaskDelayUntil( TickType_t * const pxPreviousWakeTime, const TickType_t xTimeIncrement )
{
  TickType_t xTimeToWake;
  BaseType_t xAlreadyYielded, xShouldDelay = pdFALSE;
  configASSERT( pxPreviousWakeTime );
  configASSERT( ( xTimeIncrement > 0U ) );
  configASSERT( uxSchedulerSuspended == 0 );
  vTaskSuspendAll();
  /* Minor optimisation.  The tick count cannot change in this
     block. */
  const TickType_t xConstTickCount = xTickCount;
  /* Generate the tick time at which the task wants to wake. */
  xTimeToWake = *pxPreviousWakeTime + xTimeIncrement;
  if( xConstTickCount < *pxPreviousWakeTime )
    /* The tick count has overflowed since this function was
       lasted called.  In this case the only time we should ever
       actually delay is if the wake time has also	overflowed,
       and the wake time is greater than the tick time.  When this
       is the case it is as if neither time had overflowed. */
    if( ( xTimeToWake < *pxPreviousWakeTime ) && ( xTimeToWake > xConstTickCount ) )
      xShouldDelay = pdTRUE;
    else
      mtCOVERAGE_TEST_MARKER();
  else
    /* The tick time has not overflowed.  In this case we will
       delay if either the wake time has overflowed, and/or the
       tick time is less than the wake time. */
    if( ( xTimeToWake < *pxPreviousWakeTime ) || ( xTimeToWake > xConstTickCount ) )
      xShouldDelay = pdTRUE;
    else
      mtCOVERAGE_TEST_MARKER();
  /* Update the wake time ready for the next call. */
  *pxPreviousWakeTime = xTimeToWake;
  if( xShouldDelay != pdFALSE )
    {
      traceTASK_DELAY_UNTIL( xTimeToWake );
      /* prvAddCurrentTaskToDelayedList() needs the block time, not
         the time to wake, so subtract the current tick count. */
      prvAddCurrentTaskToDelayedList( xTimeToWake - xConstTickCount, pdFALSE );
    }
  else
    mtCOVERAGE_TEST_MARKER();
  xAlreadyYielded = xTaskResumeAll();
  /* Force a reschedule if xTaskResumeAll has not already done so, we may
     have put ourselves to sleep. */
  if( xAlreadyYielded == pdFALSE )
    portYIELD_WITHIN_API();
  else
    mtCOVERAGE_TEST_MARKER();
}

void vTaskDelay( const TickType_t xTicksToDelay )
{
  BaseType_t xAlreadyYielded = pdFALSE;
  /* A delay time of zero just forces a reschedule. */
  if( xTicksToDelay > ( TickType_t ) 0U )
    {
      configASSERT( uxSchedulerSuspended == 0 );
      vTaskSuspendAll();
      traceTASK_DELAY();
      /* A task that is removed from the event list while the
         scheduler is suspended will not get placed in the ready
         list or removed from the blocked list until the scheduler
         is resumed.

         This task cannot be in an event list as it is the currently
         executing task. */
      prvAddCurrentTaskToDelayedList( xTicksToDelay, pdFALSE );
      xAlreadyYielded = xTaskResumeAll();
    }
  else
    mtCOVERAGE_TEST_MARKER();
  /* Force a reschedule if xTaskResumeAll has not already done so, we may
     have put ourselves to sleep. */
  if( xAlreadyYielded == pdFALSE )
    portYIELD_WITHIN_API();
  else
    mtCOVERAGE_TEST_MARKER();
}

/*-----------------------------------------------------------*/

eTaskState eTaskGetState( TaskHandle_t xTask )
{
  eTaskState eReturn;
  List_t const * pxStateList, *pxDelayedList, *pxOverflowedDelayedList;
  const TCB_t * const pxTCB = xTask;
  configASSERT( pxTCB );
  if( pxTCB == pxCurrentTCB )
    /* The task calling this function is querying its own state. */
    eReturn = eRunning;
  else
    {
      taskENTER_CRITICAL();
      pxStateList = listLIST_ITEM_CONTAINER( &( pxTCB->xStateListItem ) );
      pxDelayedList = pxDelayedTaskList;
      pxOverflowedDelayedList = pxOverflowDelayedTaskList;
      taskEXIT_CRITICAL();
      if( ( pxStateList == pxDelayedList ) || ( pxStateList == pxOverflowedDelayedList ) )
        /* The task being queried is referenced from one of the Blocked
           lists. */
        eReturn = eBlocked;
      else if( pxStateList == &xSuspendedTaskList )
        /* The task being queried is referenced from the suspended
           list.  Is it genuinely suspended or is it blocked
           indefinitely? */
        if( listLIST_ITEM_CONTAINER( &( pxTCB->xEventListItem ) ) == NULL )
          eReturn = eSuspended;
        else
          eReturn = eBlocked;
      else if( ( pxStateList == &xTasksWaitingTermination ) || ( pxStateList == NULL ) )
        /* The task being queried is referenced from the deleted
           tasks list, or it is not referenced from any lists at
           all. */
        eReturn = eDeleted;
      else
        /* If the task is not in any other state, it must be in the
           Ready (including pending ready) state. */
        eReturn = eReady;
    }
  return eReturn;
}
/*-----------------------------------------------------------*/

UBaseType_t uxTaskPriorityGet( const TaskHandle_t xTask )
{
  TCB_t const *pxTCB;
  UBaseType_t uxReturn;
  taskENTER_CRITICAL();
  /* If null is passed in here then it is the priority of the task
     that called uxTaskPriorityGet() that is being queried. */
  pxTCB = prvGetTCBFromHandle( xTask );
  uxReturn = pxTCB->uxPriority;
  taskEXIT_CRITICAL();
  return uxReturn;
}
/*-----------------------------------------------------------*/

UBaseType_t uxTaskPriorityGetFromISR( const TaskHandle_t xTask )
{
  TCB_t const *pxTCB;
  UBaseType_t uxReturn, uxSavedInterruptState;
  /* RTOS ports that support interrupt nesting have the concept of a
     maximum	system call (or maximum API call) interrupt priority.
     Interrupts that are	above the maximum system call priority are keep
     permanently enabled, even when the RTOS kernel is in a critical section,
     but cannot make any calls to FreeRTOS API functions.  If configASSERT()
     is defined in FreeRTOSConfig.h then
     portASSERT_IF_INTERRUPT_PRIORITY_INVALID() will result in an assertion
     failure if a FreeRTOS API function is called from an interrupt that has
     been assigned a priority above the configured maximum system call
     priority.  Only FreeRTOS functions that end in FromISR can be called
     from interrupts	that have been assigned a priority at or (logically)
     below the maximum system call interrupt priority.  FreeRTOS maintains a
     separate interrupt safe API to ensure interrupt entry is as fast and as
     simple as possible.  More information (albeit Cortex-M specific) is
     provided on the following link:
     https://www.freertos.org/RTOS-Cortex-M3-M4.html */
  portASSERT_IF_INTERRUPT_PRIORITY_INVALID();
  uxSavedInterruptState = portSET_INTERRUPT_MASK_FROM_ISR();
  /* If null is passed in here then it is the priority of the calling
     task that is being queried. */
  pxTCB = prvGetTCBFromHandle( xTask );
  uxReturn = pxTCB->uxPriority;
  portCLEAR_INTERRUPT_MASK_FROM_ISR( uxSavedInterruptState );
  return uxReturn;
}
/*-----------------------------------------------------------*/

void vTaskPrioritySet( TaskHandle_t xTask, UBaseType_t uxNewPriority )
{
  TCB_t *pxTCB;
  UBaseType_t uxCurrentBasePriority, uxPriorityUsedOnEntry;
  BaseType_t xYieldRequired = pdFALSE;
  configASSERT( ( uxNewPriority < configMAX_PRIORITIES ) );
  /* Ensure the new priority is valid. */
  if( uxNewPriority >= ( UBaseType_t ) configMAX_PRIORITIES )
    uxNewPriority = ( UBaseType_t ) configMAX_PRIORITIES - ( UBaseType_t ) 1U;
  else
    mtCOVERAGE_TEST_MARKER();
  taskENTER_CRITICAL();
  /* If null is passed in here then it is the priority of the calling
     task that is being changed. */
  pxTCB = prvGetTCBFromHandle( xTask );
  traceTASK_PRIORITY_SET( pxTCB, uxNewPriority );
  uxCurrentBasePriority = pxTCB->uxPriority;
  if( uxCurrentBasePriority != uxNewPriority )
    {
      /* The priority change may have readied a task of higher
         priority than the calling task. */
      if( uxNewPriority > uxCurrentBasePriority )
        if( pxTCB != pxCurrentTCB )
          /* The priority of a task other than the currently
             running task is being raised.  Is the priority being
             raised above that of the running task? */
          if( uxNewPriority >= pxCurrentTCB->uxPriority )
            xYieldRequired = pdTRUE;
          else
            mtCOVERAGE_TEST_MARKER();
        else
          {
            /* The priority of the running task is being raised,
               but the running task must already be the highest
               priority task able to run so no yield is required. */
          }
      else if( pxTCB == pxCurrentTCB )
        /* Setting the priority of the running task down means
           there may now be another task of higher priority that
           is ready to execute. */
        xYieldRequired = pdTRUE;
      else
        {
          /* Setting the priority of any other task down does not
             require a yield as the running task must be above the
             new priority of the task being modified. */
        }
      /* Remember the ready list the task might be referenced from
         before its uxPriority member is changed so the
         taskRESET_READY_PRIORITY() macro can function correctly. */
      uxPriorityUsedOnEntry = pxTCB->uxPriority;
      pxTCB->uxPriority = uxNewPriority;
      /* Only reset the event list item value if the value is not
         being used for anything else. */
      if( ( listGET_LIST_ITEM_VALUE( &( pxTCB->xEventListItem ) ) & taskEVENT_LIST_ITEM_VALUE_IN_USE ) == 0UL )
        listSET_LIST_ITEM_VALUE( &( pxTCB->xEventListItem ), ( ( TickType_t ) configMAX_PRIORITIES - ( TickType_t ) uxNewPriority ) );
      else
        mtCOVERAGE_TEST_MARKER();
      /* If the task is in the blocked or suspended list we need do
         nothing more than change its priority variable. However, if
         the task is in a ready list it needs to be removed and placed
         in the list appropriate to its new priority. */
      if( listIS_CONTAINED_WITHIN( &( pxReadyTasksLists[ uxPriorityUsedOnEntry ] ), &( pxTCB->xStateListItem ) ) != pdFALSE )
        {
          /* The task is currently in its ready list - remove before
             adding it to it's new ready list.  As we are in a critical
             section we can do this even if the scheduler is suspended. */
          if( uxListRemove( &( pxTCB->xStateListItem ) ) == ( UBaseType_t ) 0 )
            /* It is known that the task is in its ready list so
               there is no need to check again and the port level
               reset macro can be called directly. */
            portRESET_READY_PRIORITY( uxPriorityUsedOnEntry, uxTopReadyPriority );
          else
            mtCOVERAGE_TEST_MARKER();
          prvAddTaskToReadyList( pxTCB );
        }
      else
        mtCOVERAGE_TEST_MARKER();
      if( xYieldRequired != pdFALSE )
        taskYIELD_IF_USING_PREEMPTION();
      else
        mtCOVERAGE_TEST_MARKER();
      /* Remove compiler warning about unused variables when the port
         optimised task selection is not being used. */
      ( void ) uxPriorityUsedOnEntry;
    }
  taskEXIT_CRITICAL();
}
/*-----------------------------------------------------------*/

void vTaskSuspend( TaskHandle_t xTaskToSuspend )
{
  TCB_t *pxTCB;
  taskENTER_CRITICAL();
  /* If null is passed in here then it is the running task that is
     being suspended. */
  pxTCB = prvGetTCBFromHandle( xTaskToSuspend );
  traceTASK_SUSPEND( pxTCB );
  /* Remove task from the ready/delayed list and place in the
     suspended list. */
  if( uxListRemove( &( pxTCB->xStateListItem ) ) == ( UBaseType_t ) 0 )
    taskRESET_READY_PRIORITY( pxTCB->uxPriority );
  else
    mtCOVERAGE_TEST_MARKER();
  /* Is the task waiting on an event also? */
  if( listLIST_ITEM_CONTAINER( &( pxTCB->xEventListItem ) ) != NULL )
    ( void ) uxListRemove( &( pxTCB->xEventListItem ) );
  else
    mtCOVERAGE_TEST_MARKER();
  vListInsertEnd( &xSuspendedTaskList, &( pxTCB->xStateListItem ) );
  taskEXIT_CRITICAL();
  if( xSchedulerRunning != pdFALSE )
    {
      /* Reset the next expected unblock time in case it referred to the
         task that is now in the Suspended state. */
      taskENTER_CRITICAL();
      prvResetNextTaskUnblockTime();
      taskEXIT_CRITICAL();
    }
  else
    mtCOVERAGE_TEST_MARKER();
  if( pxTCB == pxCurrentTCB )
    if( xSchedulerRunning != pdFALSE )
      {
        /* The current task has just been suspended. */
        configASSERT( uxSchedulerSuspended == 0 );
        portYIELD_WITHIN_API();
      }
    else
      /* The scheduler is not running, but the task that was pointed
         to by pxCurrentTCB has just been suspended and pxCurrentTCB
         must be adjusted to point to a different task. */
      if( listCURRENT_LIST_LENGTH( &xSuspendedTaskList ) == uxCurrentNumberOfTasks )
        /* No other tasks are ready, so set pxCurrentTCB back to
           NULL so when the next task is created pxCurrentTCB will
           be set to point to it no matter what its relative priority
           is. */
        pxCurrentTCB = NULL;
      else
        vTaskSwitchContext();
  else
    mtCOVERAGE_TEST_MARKER();
}

static BaseType_t prvTaskIsTaskSuspended( const TaskHandle_t xTask )
{
  BaseType_t xReturn = pdFALSE;
  const TCB_t * const pxTCB = xTask;
  /* Accesses xPendingReadyList so must be called from a critical
     section. */
  /* It does not make sense to check if the calling task is suspended. */
  configASSERT( xTask );
  /* Is the task being resumed actually in the suspended list? */
  if( listIS_CONTAINED_WITHIN( &xSuspendedTaskList, &( pxTCB->xStateListItem ) ) != pdFALSE )
    /* Has the task already been resumed from within an ISR? */
    if( listIS_CONTAINED_WITHIN( &xPendingReadyList, &( pxTCB->xEventListItem ) ) == pdFALSE )
      /* Is it in the suspended list because it is in the	Suspended
         state, or because is is blocked with no timeout? */
      if( listIS_CONTAINED_WITHIN( NULL, &( pxTCB->xEventListItem ) ) != pdFALSE )
        xReturn = pdTRUE;
      else
        mtCOVERAGE_TEST_MARKER();
    else
      mtCOVERAGE_TEST_MARKER();
  else
    mtCOVERAGE_TEST_MARKER();
  return xReturn;
}

void vTaskResume( TaskHandle_t xTaskToResume )
{
  TCB_t * const pxTCB = xTaskToResume;
  /* It does not make sense to resume the calling task. */
  configASSERT( xTaskToResume );
  /* The parameter cannot be NULL as it is impossible to resume the
     currently executing task. */
  if( ( pxTCB != pxCurrentTCB ) && ( pxTCB != NULL ) )
    {
      taskENTER_CRITICAL();
      if( prvTaskIsTaskSuspended( pxTCB ) != pdFALSE )
        {
          traceTASK_RESUME( pxTCB );
          /* The ready list can be accessed even if the scheduler is
             suspended because this is inside a critical section. */
          ( void ) uxListRemove(  &( pxTCB->xStateListItem ) );
          prvAddTaskToReadyList( pxTCB );
          /* A higher priority task may have just been resumed. */
          if( pxTCB->uxPriority >= pxCurrentTCB->uxPriority )
            /* This yield may not cause the task just resumed to run,
               but will leave the lists in the correct state for the
               next yield. */
            taskYIELD_IF_USING_PREEMPTION();
          else
            mtCOVERAGE_TEST_MARKER();
        }
      else
        mtCOVERAGE_TEST_MARKER();
      taskEXIT_CRITICAL();
    }
  else
    mtCOVERAGE_TEST_MARKER();
}

void vTaskStartScheduler( void )
{
  BaseType_t xReturn;
  /* Add the idle task at the lowest priority. */
  StaticTask_t *pxIdleTaskTCBBuffer = NULL;
  StackType_t *pxIdleTaskStackBuffer = NULL;
  uint32_t ulIdleTaskStackSize;
  /* The Idle task is created using user provided RAM - obtain the
     address of the RAM then create the idle task. */
  vApplicationGetIdleTaskMemory( &pxIdleTaskTCBBuffer, &pxIdleTaskStackBuffer, &ulIdleTaskStackSize );
  xIdleTaskHandle = xTaskCreateStatic(	prvIdleTask,
                                        configIDLE_TASK_NAME,
                                        ulIdleTaskStackSize,
                                        ( void * ) NULL,
                                        portPRIVILEGE_BIT, /* In effect ( tskIDLE_PRIORITY | portPRIVILEGE_BIT ), but tskIDLE_PRIORITY is zero. */
                                        pxIdleTaskStackBuffer,
                                        pxIdleTaskTCBBuffer );
  if( xIdleTaskHandle != NULL )
    xReturn = pdPASS;
  else
    xReturn = pdFAIL;
  if( xReturn == pdPASS )
    xReturn = xTimerCreateTimerTask();
  else
    mtCOVERAGE_TEST_MARKER();
  if( xReturn == pdPASS )
    {
      /* Interrupts are turned off here, to ensure a tick does not occur
         before or during the call to xPortStartScheduler().  The stacks of
         the created tasks contain a status word with interrupts switched on
         so interrupts will automatically get re-enabled when the first task
         starts to run. */
      portDISABLE_INTERRUPTS();
      xNextTaskUnblockTime = portMAX_DELAY;
      xSchedulerRunning = pdTRUE;
      xTickCount = ( TickType_t ) configINITIAL_TICK_COUNT;
      traceTASK_SWITCHED_IN();
    }
  else
      /* This line will only be reached if the kernel could not be started,
         because there was not enough FreeRTOS heap to create the idle task
         or the timer task. */
      configASSERT( xReturn != errCOULD_NOT_ALLOCATE_REQUIRED_MEMORY );
  /* Prevent compiler warnings if INCLUDE_xTaskGetIdleTaskHandle is set to 0,
     meaning xIdleTaskHandle is not used anywhere else. */
  ( void ) xIdleTaskHandle;
}

void vTaskEndScheduler( void )
{
  /* Stop the scheduler interrupts and call the portable scheduler end
     routine so the original ISRs can be restored if necessary.  The port
     layer must ensure interrupts enable	bit is left in the correct state. */
  portDISABLE_INTERRUPTS();
  xSchedulerRunning = pdFALSE;
  vPortEndScheduler();
}

void vTaskSuspendAll( void )
{
  /* A critical section is not required as the variable is of type
     BaseType_t.  Please read Richard Barry's reply in the following link to a
     post in the FreeRTOS support forum before reporting this as a bug! -
     http://goo.gl/wu4acr */
  /* portSOFRWARE_BARRIER() is only implemented for emulated/simulated ports that
     do not otherwise exhibit real time behaviour. */
  portSOFTWARE_BARRIER();
  /* The scheduler is suspended if uxSchedulerSuspended is non-zero.  An increment
     is used to allow calls to vTaskSuspendAll() to nest. */
  ++uxSchedulerSuspended;
  /* Enforces ordering for ports and optimised compilers that may otherwise place
     the above increment elsewhere. */
  portMEMORY_BARRIER();
}

BaseType_t xTaskResumeAll( void )
{
  TCB_t *pxTCB = NULL;
  BaseType_t xAlreadyYielded = pdFALSE;
  /* If uxSchedulerSuspended is zero then this function does not match a
     previous call to vTaskSuspendAll(). */
  configASSERT( uxSchedulerSuspended );
  /* It is possible that an ISR caused a task to be removed from an event
     list while the scheduler was suspended.  If this was the case then the
     removed task will have been added to the xPendingReadyList.  Once the
     scheduler has been resumed it is safe to move all the pending ready
     tasks from this list into their appropriate ready list. */
  taskENTER_CRITICAL();
  --uxSchedulerSuspended;
  if( uxSchedulerSuspended == ( UBaseType_t ) pdFALSE )
    {
      if( uxCurrentNumberOfTasks > ( UBaseType_t ) 0U )
        {
          /* Move any readied tasks from the pending list into the
             appropriate ready list. */
          while( listLIST_IS_EMPTY( &xPendingReadyList ) == pdFALSE )
            {
              pxTCB = listGET_OWNER_OF_HEAD_ENTRY( ( &xPendingReadyList ) );
              ( void ) uxListRemove( &( pxTCB->xEventListItem ) );
              ( void ) uxListRemove( &( pxTCB->xStateListItem ) );
              prvAddTaskToReadyList( pxTCB );
              /* If the moved task has a priority higher than the current
                 task then a yield must be performed. */
              if( pxTCB->uxPriority >= pxCurrentTCB->uxPriority )
                xYieldPending = pdTRUE;
              else
                mtCOVERAGE_TEST_MARKER();
            }
          if( pxTCB != NULL )
            {
              /* A task was unblocked while the scheduler was suspended,
                 which may have prevented the next unblock time from being
                 re-calculated, in which case re-calculate it now.  Mainly
                 important for low power tickless implementations, where
                 this can prevent an unnecessary exit from low power
                 state. */
              prvResetNextTaskUnblockTime();
            }
          /* If any ticks occurred while the scheduler was suspended then
             they should be processed now.  This ensures the tick count does
             not	slip, and that any delayed tasks are resumed at the correct
             time. */
          {
            TickType_t xPendedCounts = xPendedTicks; /* Non-volatile copy. */
            if( xPendedCounts > ( TickType_t ) 0U )
              {
                do
                  {
                    if( xTaskIncrementTick() != pdFALSE )
                      xYieldPending = pdTRUE;
                    else
                      mtCOVERAGE_TEST_MARKER();
                    --xPendedCounts;
                  } while( xPendedCounts > ( TickType_t ) 0U );
                xPendedTicks = 0;
              }
            else
              mtCOVERAGE_TEST_MARKER();
          }
          if( xYieldPending != pdFALSE )
            taskYIELD_IF_USING_PREEMPTION();
          else
            mtCOVERAGE_TEST_MARKER();
        }
    }
  else
    mtCOVERAGE_TEST_MARKER();
  taskEXIT_CRITICAL();
  return xAlreadyYielded;
}

TickType_t xTaskGetTickCount( void )
{
  TickType_t xTicks;
  /* Critical section required if running on a 16 bit processor. */
  portTICK_TYPE_ENTER_CRITICAL();
  xTicks = xTickCount;
  portTICK_TYPE_EXIT_CRITICAL();
  return xTicks;
}

TickType_t xTaskGetTickCountFromISR( void )
{
  TickType_t xReturn;
  UBaseType_t uxSavedInterruptStatus;
  /* RTOS ports that support interrupt nesting have the concept of a maximum
     system call (or maximum API call) interrupt priority.  Interrupts that are
     above the maximum system call priority are kept permanently enabled, even
     when the RTOS kernel is in a critical section, but cannot make any calls to
     FreeRTOS API functions.  If configASSERT() is defined in FreeRTOSConfig.h
     then portASSERT_IF_INTERRUPT_PRIORITY_INVALID() will result in an assertion
     failure if a FreeRTOS API function is called from an interrupt that has been
     assigned a priority above the configured maximum system call priority.
     Only FreeRTOS functions that end in FromISR can be called from interrupts
     that have been assigned a priority at or (logically) below the maximum
     system call	interrupt priority.  FreeRTOS maintains a separate interrupt
     safe API to ensure interrupt entry is as fast and as simple as possible.
     More information (albeit Cortex-M specific) is provided on the following
     link: https://www.freertos.org/RTOS-Cortex-M3-M4.html */
  portASSERT_IF_INTERRUPT_PRIORITY_INVALID();
  uxSavedInterruptStatus = portTICK_TYPE_SET_INTERRUPT_MASK_FROM_ISR();
  xReturn = xTickCount;
  portTICK_TYPE_CLEAR_INTERRUPT_MASK_FROM_ISR( uxSavedInterruptStatus );
  return xReturn;
}

UBaseType_t uxTaskGetNumberOfTasks( void )
{
  /* A critical section is not required because the variables are of type
     BaseType_t. */
  return uxCurrentNumberOfTasks;
}

char *pcTaskGetName( TaskHandle_t xTaskToQuery )
{
  TCB_t *pxTCB;
  /* If null is passed in here then the name of the calling task is being
     queried. */
  pxTCB = prvGetTCBFromHandle( xTaskToQuery );
  configASSERT( pxTCB );
  return &( pxTCB->pcTaskName[ 0 ] );
}

static TCB_t *prvSearchForNameWithinSingleList( List_t *pxList, const char pcNameToQuery[] )
{
  TCB_t *pxNextTCB, *pxFirstTCB, *pxReturn = NULL;
  UBaseType_t x;
  char cNextChar;
  BaseType_t xBreakLoop;
  /* This function is called with the scheduler suspended. */
  if( listCURRENT_LIST_LENGTH( pxList ) > ( UBaseType_t ) 0 )
    {
      listGET_OWNER_OF_NEXT_ENTRY( pxFirstTCB, pxList );
      do
        {
          listGET_OWNER_OF_NEXT_ENTRY( pxNextTCB, pxList );
          /* Check each character in the name looking for a match or
             mismatch. */
          xBreakLoop = pdFALSE;
          for( x = ( UBaseType_t ) 0; x < ( UBaseType_t ) configMAX_TASK_NAME_LEN; x++ )
            {
              cNextChar = pxNextTCB->pcTaskName[ x ];
              if( cNextChar != pcNameToQuery[ x ] )
                /* Characters didn't match. */
                xBreakLoop = pdTRUE;
              else if( cNextChar == ( char ) 0x00 )
                {
                  /* Both strings terminated, a match must have been
                     found. */
                  pxReturn = pxNextTCB;
                  xBreakLoop = pdTRUE;
                }
              else
                mtCOVERAGE_TEST_MARKER();
              if( xBreakLoop != pdFALSE )
                break;
            }
          if( pxReturn != NULL )
            /* The handle has been found. */
            break;
        } while( pxNextTCB != pxFirstTCB );
    }
  else
    mtCOVERAGE_TEST_MARKER();
  return pxReturn;
}

TaskHandle_t xTaskGetHandle( const char *pcNameToQuery )
{
  UBaseType_t uxQueue = configMAX_PRIORITIES;
  TCB_t* pxTCB;
  /* Task names will be truncated to configMAX_TASK_NAME_LEN - 1 bytes. */
  configASSERT( strlen( pcNameToQuery ) < configMAX_TASK_NAME_LEN );
  vTaskSuspendAll();
  {
    /* Search the ready lists. */
    do
      {
        uxQueue--;
        pxTCB = prvSearchForNameWithinSingleList( ( List_t * ) &( pxReadyTasksLists[ uxQueue ] ), pcNameToQuery );
        if( pxTCB != NULL )
          /* Found the handle. */
          break;
      } while( uxQueue > ( UBaseType_t ) tskIDLE_PRIORITY );
    /* Search the delayed lists. */
    if( pxTCB == NULL )
      pxTCB = prvSearchForNameWithinSingleList( ( List_t * ) pxDelayedTaskList, pcNameToQuery );
    if( pxTCB == NULL )
      pxTCB = prvSearchForNameWithinSingleList( ( List_t * ) pxOverflowDelayedTaskList, pcNameToQuery );
    if( pxTCB == NULL )
      /* Search the suspended list. */
      pxTCB = prvSearchForNameWithinSingleList( &xSuspendedTaskList, pcNameToQuery );
    if( pxTCB == NULL )
      /* Search the deleted list. */
      pxTCB = prvSearchForNameWithinSingleList( &xTasksWaitingTermination, pcNameToQuery );
  }
  ( void ) xTaskResumeAll();
  return pxTCB;
}

TaskHandle_t xTaskGetIdleTaskHandle( void )
{
  /* If xTaskGetIdleTaskHandle() is called before the scheduler has been
     started, then xIdleTaskHandle will be NULL. */
  configASSERT( ( xIdleTaskHandle != NULL ) );
  return xIdleTaskHandle;
}

BaseType_t xTaskCatchUpTicks( TickType_t xTicksToCatchUp )
{
  BaseType_t xYieldRequired = pdFALSE;
  /* Must not be called with the scheduler suspended as the implementation
     relies on xPendedTicks being wound down to 0 in xTaskResumeAll(). */
  configASSERT( uxSchedulerSuspended == 0 );
  /* Use xPendedTicks to mimic xTicksToCatchUp number of ticks occurring when
     the scheduler is suspended so the ticks are executed in xTaskResumeAll(). */
  vTaskSuspendAll();
  xPendedTicks += xTicksToCatchUp;
  xYieldRequired = xTaskResumeAll();
  return xYieldRequired;
}

BaseType_t xTaskIncrementTick( void )
{
  TCB_t * pxTCB;
  TickType_t xItemValue;
  BaseType_t xSwitchRequired = pdFALSE;
  /* Called by the portable layer each time a tick interrupt occurs.
     Increments the tick then checks to see if the new tick value will cause any
     tasks to be unblocked. */
  traceTASK_INCREMENT_TICK( xTickCount );
  if( uxSchedulerSuspended == ( UBaseType_t ) pdFALSE )
    {
      /* Minor optimisation.  The tick count cannot change in this
         block. */
      const TickType_t xConstTickCount = xTickCount + ( TickType_t ) 1;
      /* Increment the RTOS tick, switching the delayed and overflowed
         delayed lists if it wraps to 0. */
      xTickCount = xConstTickCount;
      if( xConstTickCount == ( TickType_t ) 0U )
        taskSWITCH_DELAYED_LISTS();
      else
        mtCOVERAGE_TEST_MARKER();
      /* See if this tick has made a timeout expire.  Tasks are stored in
         the	queue in the order of their wake time - meaning once one task
         has been found whose block time has not expired there is no need to
         look any further down the list. */
      if( xConstTickCount >= xNextTaskUnblockTime )
        {
          for( ;; )
            {
              if( listLIST_IS_EMPTY( pxDelayedTaskList ) != pdFALSE )
                {
                  /* The delayed list is empty.  Set xNextTaskUnblockTime
                     to the maximum possible value so it is extremely
                     unlikely that the
                     if( xTickCount >= xNextTaskUnblockTime ) test will pass
                     next time through. */
                  xNextTaskUnblockTime = portMAX_DELAY;
                  break;
                }
              else
                {
                  /* The delayed list is not empty, get the value of the
                     item at the head of the delayed list.  This is the time
                     at which the task at the head of the delayed list must
                     be removed from the Blocked state. */
                  pxTCB = listGET_OWNER_OF_HEAD_ENTRY( pxDelayedTaskList );
                  xItemValue = listGET_LIST_ITEM_VALUE( &( pxTCB->xStateListItem ) );
                  if( xConstTickCount < xItemValue )
                    {
                      /* It is not time to unblock this item yet, but the
                         item value is the time at which the task at the head
                         of the blocked list must be removed from the Blocked
                         state -	so record the item value in
                         xNextTaskUnblockTime. */
                      xNextTaskUnblockTime = xItemValue;
                      break;
                    }
                  else
                    mtCOVERAGE_TEST_MARKER();
                  /* It is time to remove the item from the Blocked state. */
                  ( void ) uxListRemove( &( pxTCB->xStateListItem ) );
                  /* Is the task waiting on an event also?  If so remove
                     it from the event list. */
                  if( listLIST_ITEM_CONTAINER( &( pxTCB->xEventListItem ) ) != NULL )
                    ( void ) uxListRemove( &( pxTCB->xEventListItem ) );
                  else
                    mtCOVERAGE_TEST_MARKER();
                  /* Place the unblocked task into the appropriate ready
                     list. */
                  prvAddTaskToReadyList( pxTCB );
                  /* A task being unblocked cannot cause an immediate
                     context switch if preemption is turned off. */
                }
            }
        }
    }
  else
    ++xPendedTicks;
  return xSwitchRequired;
}

void vTaskSwitchContext( void )
{
  if( uxSchedulerSuspended != ( UBaseType_t ) pdFALSE )
    /* The scheduler is currently suspended - do not allow a context
       switch. */
    xYieldPending = pdTRUE;
  else
    {
      xYieldPending = pdFALSE;
      traceTASK_SWITCHED_OUT();
      /* Check for stack overflow, if configured. */
      taskCHECK_FOR_STACK_OVERFLOW();
      /* Before the currently running task is switched out, save its errno. */
      pxCurrentTCB->iTaskErrno = FreeRTOS_errno;
      /* Select a new task to run using either the generic C or port
         optimised asm code. */
      taskSELECT_HIGHEST_PRIORITY_TASK();
      traceTASK_SWITCHED_IN();
      /* After the new task is switched in, update the global errno. */
      FreeRTOS_errno = pxCurrentTCB->iTaskErrno;
    }
}

void vTaskPlaceOnEventList( List_t * const pxEventList, const TickType_t xTicksToWait )
{
  configASSERT( pxEventList );
  /* THIS FUNCTION MUST BE CALLED WITH EITHER INTERRUPTS DISABLED OR THE
     SCHEDULER SUSPENDED AND THE QUEUE BEING ACCESSED LOCKED. */
  /* Place the event list item of the TCB in the appropriate event list.
     This is placed in the list in priority order so the highest priority task
     is the first to be woken by the event.  The queue that contains the event
     list is locked, preventing simultaneous access from interrupts. */
  vListInsert( pxEventList, &( pxCurrentTCB->xEventListItem ) );
  prvAddCurrentTaskToDelayedList( xTicksToWait, pdTRUE );
}

void vTaskPlaceOnUnorderedEventList( List_t * pxEventList, const TickType_t xItemValue, const TickType_t xTicksToWait )
{
  configASSERT( pxEventList );
  /* THIS FUNCTION MUST BE CALLED WITH THE SCHEDULER SUSPENDED.  It is used by
     the event groups implementation. */
  configASSERT( uxSchedulerSuspended != 0 );
  /* Store the item value in the event list item.  It is safe to access the
     event list item here as interrupts won't access the event list item of a
     task that is not in the Blocked state. */
  listSET_LIST_ITEM_VALUE( &( pxCurrentTCB->xEventListItem ), xItemValue | taskEVENT_LIST_ITEM_VALUE_IN_USE );
  /* Place the event list item of the TCB at the end of the appropriate event
     list.  It is safe to access the event list here because it is part of an
     event group implementation - and interrupts don't access event groups
     directly (instead they access them indirectly by pending function calls to
     the task level). */
  vListInsertEnd( pxEventList, &( pxCurrentTCB->xEventListItem ) );
  prvAddCurrentTaskToDelayedList( xTicksToWait, pdTRUE );
}

void vTaskPlaceOnEventListRestricted( List_t * const pxEventList, TickType_t xTicksToWait, const BaseType_t xWaitIndefinitely )
{
  configASSERT( pxEventList );
  /* This function should not be called by application code hence the
     'Restricted' in its name.  It is not part of the public API.  It is
     designed for use by kernel code, and has special calling requirements -
     it should be called with the scheduler suspended. */
  /* Place the event list item of the TCB in the appropriate event list.
     In this case it is assume that this is the only task that is going to
     be waiting on this event list, so the faster vListInsertEnd() function
     can be used in place of vListInsert. */
  vListInsertEnd( pxEventList, &( pxCurrentTCB->xEventListItem ) );
  /* If the task should block indefinitely then set the block time to a
     value that will be recognised as an indefinite delay inside the
     prvAddCurrentTaskToDelayedList() function. */
  if( xWaitIndefinitely != pdFALSE )
    xTicksToWait = portMAX_DELAY;
  traceTASK_DELAY_UNTIL( ( xTickCount + xTicksToWait ) );
  prvAddCurrentTaskToDelayedList( xTicksToWait, xWaitIndefinitely );
}

BaseType_t xTaskRemoveFromEventList( const List_t * const pxEventList )
{
  TCB_t *pxUnblockedTCB;
  BaseType_t xReturn;

  /* THIS FUNCTION MUST BE CALLED FROM A CRITICAL SECTION.  It can also be
     called from a critical section within an ISR. */

  /* The event list is sorted in priority order, so the first in the list can
     be removed as it is known to be the highest priority.  Remove the TCB from
     the delayed list, and add it to the ready list.

     If an event is for a queue that is locked then this function will never
     get called - the lock count on the queue will get modified instead.  This
     means exclusive access to the event list is guaranteed here.

     This function assumes that a check has already been made to ensure that
     pxEventList is not empty. */
  pxUnblockedTCB = listGET_OWNER_OF_HEAD_ENTRY( pxEventList );
  configASSERT( pxUnblockedTCB );
  ( void ) uxListRemove( &( pxUnblockedTCB->xEventListItem ) );
  if( uxSchedulerSuspended == ( UBaseType_t ) pdFALSE )
    {
      ( void ) uxListRemove( &( pxUnblockedTCB->xStateListItem ) );
      prvAddTaskToReadyList( pxUnblockedTCB );
    }
  else
    /* The delayed and ready lists cannot be accessed, so hold this task
       pending until the scheduler is resumed. */
    vListInsertEnd( &( xPendingReadyList ), &( pxUnblockedTCB->xEventListItem ) );
  if( pxUnblockedTCB->uxPriority > pxCurrentTCB->uxPriority )
    {
      /* Return true if the task removed from the event list has a higher
         priority than the calling task.  This allows the calling task to know if
         it should force a context switch now. */
      xReturn = pdTRUE;
      /* Mark that a yield is pending in case the user is not using the
         "xHigherPriorityTaskWoken" parameter to an ISR safe FreeRTOS function. */
      xYieldPending = pdTRUE;
    }
  else
    xReturn = pdFALSE;
  return xReturn;
}

void vTaskRemoveFromUnorderedEventList( ListItem_t * pxEventListItem, const TickType_t xItemValue )
{
  TCB_t *pxUnblockedTCB;
  /* THIS FUNCTION MUST BE CALLED WITH THE SCHEDULER SUSPENDED.  It is used by
     the event flags implementation. */
  configASSERT( uxSchedulerSuspended != pdFALSE );
  /* Store the new item value in the event list. */
  listSET_LIST_ITEM_VALUE( pxEventListItem, xItemValue | taskEVENT_LIST_ITEM_VALUE_IN_USE );
  /* Remove the event list form the event flag.  Interrupts do not access
     event flags. */
  pxUnblockedTCB = listGET_LIST_ITEM_OWNER( pxEventListItem );
  configASSERT( pxUnblockedTCB );
  ( void ) uxListRemove( pxEventListItem );
  /* Remove the task from the delayed list and add it to the ready list.  The
     scheduler is suspended so interrupts will not be accessing the ready
     lists. */
  ( void ) uxListRemove( &( pxUnblockedTCB->xStateListItem ) );
  prvAddTaskToReadyList( pxUnblockedTCB );
  if( pxUnblockedTCB->uxPriority > pxCurrentTCB->uxPriority )
    /* The unblocked task has a priority above that of the calling task, so
       a context switch is required.  This function is called with the
       scheduler suspended so xYieldPending is set so the context switch
       occurs immediately that the scheduler is resumed (unsuspended). */
    xYieldPending = pdTRUE;
}

void vTaskSetTimeOutState( TimeOut_t * const pxTimeOut )
{
  configASSERT( pxTimeOut );
  taskENTER_CRITICAL();
  pxTimeOut->xOverflowCount = xNumOfOverflows;
  pxTimeOut->xTimeOnEntering = xTickCount;
  taskEXIT_CRITICAL();
}

void vTaskInternalSetTimeOutState( TimeOut_t * const pxTimeOut )
{
  /* For internal use only as it does not use a critical section. */
  pxTimeOut->xOverflowCount = xNumOfOverflows;
  pxTimeOut->xTimeOnEntering = xTickCount;
}

BaseType_t xTaskCheckForTimeOut( TimeOut_t * const pxTimeOut, TickType_t * const pxTicksToWait )
{
  BaseType_t xReturn;
  configASSERT( pxTimeOut );
  configASSERT( pxTicksToWait );
  taskENTER_CRITICAL();
  /* Minor optimisation.  The tick count cannot change in this block. */
  const TickType_t xConstTickCount = xTickCount;
  const TickType_t xElapsedTime = xConstTickCount - pxTimeOut->xTimeOnEntering;
  if( *pxTicksToWait == portMAX_DELAY )
    /* If INCLUDE_vTaskSuspend is set to 1 and the block time
       specified is the maximum block time then the task should block
       indefinitely, and therefore never time out. */
    xReturn = pdFALSE;
  else
    if( ( xNumOfOverflows != pxTimeOut->xOverflowCount ) && ( xConstTickCount >= pxTimeOut->xTimeOnEntering ) )
      /* The tick count is greater than the time at which
         vTaskSetTimeout() was called, but has also overflowed since
         vTaskSetTimeOut() was called.  It must have wrapped all the way
         around and gone past again. This passed since vTaskSetTimeout()
         was called. */
      xReturn = pdTRUE;
    else if( xElapsedTime < *pxTicksToWait )
      {
        /* Not a genuine timeout. Adjust parameters for time remaining. */
        *pxTicksToWait -= xElapsedTime;
        vTaskInternalSetTimeOutState( pxTimeOut );
        xReturn = pdFALSE;
      }
    else
      {
        *pxTicksToWait = 0;
        xReturn = pdTRUE;
      }
  taskEXIT_CRITICAL();
  return xReturn;
}

void vTaskMissedYield( void )
{
  xYieldPending = pdTRUE;
}

/*
 * -----------------------------------------------------------
 * The Idle task.
 * ----------------------------------------------------------
 *
 * The portTASK_FUNCTION() macro is used to allow port/compiler specific
 * language extensions.  The equivalent prototype for this function is:
 *
 * void prvIdleTask( void *pvParameters );
 *
 */
static portTASK_FUNCTION( prvIdleTask, pvParameters )
{
  /* Stop warnings. */
  ( void ) pvParameters;
  /** THIS IS THE RTOS IDLE TASK - WHICH IS CREATED AUTOMATICALLY WHEN THE
      SCHEDULER IS STARTED. **/
  /* In case a task that has a secure context deletes itself, in which case
     the idle task is responsible for deleting the task's secure context, if
     any. */
  portALLOCATE_SECURE_CONTEXT( configMINIMAL_SECURE_STACK_SIZE );
  for( ;; )
    {
      /* See if any tasks have deleted themselves - if so then the idle task
         is responsible for freeing the deleted task's TCB and stack. */
      prvCheckTasksWaitingTermination();
      /* If we are not using preemption we keep forcing a task switch to
         see if any other task has become available.  If we are using
         preemption we don't need to do this as any task becoming available
         will automatically get the processor anyway. */
      taskYIELD();
    }
}

void vTaskAllocateMPURegions( TaskHandle_t xTaskToModify, const MemoryRegion_t * const xRegions )
{
  TCB_t *pxTCB;
  /* If null is passed in here then we are modifying the MPU settings of
     the calling task. */
  pxTCB = prvGetTCBFromHandle( xTaskToModify );
  vPortStoreTaskMPUSettings( &( pxTCB->xMPUSettings ), xRegions, NULL, 0 );
}

static void prvInitialiseTaskLists( void )
{
  UBaseType_t uxPriority;
  for( uxPriority = ( UBaseType_t ) 0U; uxPriority < ( UBaseType_t ) configMAX_PRIORITIES; uxPriority++ )
    {
      vListInitialise( &( pxReadyTasksLists[ uxPriority ] ) );
    }
  vListInitialise( &xDelayedTaskList1 );
  vListInitialise( &xDelayedTaskList2 );
  vListInitialise( &xPendingReadyList );
  vListInitialise( &xTasksWaitingTermination );
  vListInitialise( &xSuspendedTaskList );
  /* Start with pxDelayedTaskList using list1 and the pxOverflowDelayedTaskList
     using list2. */
  pxDelayedTaskList = &xDelayedTaskList1;
  pxOverflowDelayedTaskList = &xDelayedTaskList2;
}

static void prvCheckTasksWaitingTermination( void )
{
  /** THIS FUNCTION IS CALLED FROM THE RTOS IDLE TASK **/
  TCB_t *pxTCB;
  /* uxDeletedTasksWaitingCleanUp is used to prevent taskENTER_CRITICAL()
     being called too often in the idle task. */
  while( uxDeletedTasksWaitingCleanUp > ( UBaseType_t ) 0U )
    {
      taskENTER_CRITICAL();
      pxTCB = listGET_OWNER_OF_HEAD_ENTRY( ( &xTasksWaitingTermination ) );
      ( void ) uxListRemove( &( pxTCB->xStateListItem ) );
      --uxCurrentNumberOfTasks;
      --uxDeletedTasksWaitingCleanUp;
      taskEXIT_CRITICAL();
      prvDeleteTCB( pxTCB );
    }
}

static void prvDeleteTCB( TCB_t *pxTCB )
{
  /* This call is required specifically for the TriCore port.  It must be
     above the vPortFree() calls.  The call is also used by ports/demos that
     want to allocate and clean RAM statically. */
  portCLEAN_UP_TCB( pxTCB );
  /* Free up the memory allocated by the scheduler for the task.  It is up
     to the task to free any memory allocated at the application level.
     See the third party link http://www.nadler.com/embedded/newlibAndFreeRTOS.html
     for additional information. */
  /* The task could have been allocated statically or dynamically, so
     check what was statically allocated before trying to free the
     memory. */
  if( pxTCB->ucStaticallyAllocated == tskDYNAMICALLY_ALLOCATED_STACK_AND_TCB )
    {
      /* Both the stack and TCB were allocated dynamically, so both
         must be freed. */
      vPortFree( pxTCB->pxStack );
      vPortFree( pxTCB );
    }
  else if( pxTCB->ucStaticallyAllocated == tskSTATICALLY_ALLOCATED_STACK_ONLY )
    /* Only the stack was statically allocated, so the TCB is the
       only memory that must be freed. */
      vPortFree( pxTCB );
  else
    {
      /* Neither the stack nor the TCB were allocated dynamically, so
         nothing needs to be freed. */
      configASSERT( pxTCB->ucStaticallyAllocated == tskSTATICALLY_ALLOCATED_STACK_AND_TCB	);
      mtCOVERAGE_TEST_MARKER();
    }
}

static void prvResetNextTaskUnblockTime( void )
{
  TCB_t *pxTCB;
  if( listLIST_IS_EMPTY( pxDelayedTaskList ) != pdFALSE )
    /* The new current delayed list is empty.  Set xNextTaskUnblockTime to
       the maximum possible value so it is	extremely unlikely that the
       if( xTickCount >= xNextTaskUnblockTime ) test will pass until
       there is an item in the delayed list. */
    xNextTaskUnblockTime = portMAX_DELAY;
  else
    {
      /* The new current delayed list is not empty, get the value of
         the item at the head of the delayed list.  This is the time at
         which the task at the head of the delayed list should be removed
         from the Blocked state. */
      ( pxTCB ) = listGET_OWNER_OF_HEAD_ENTRY( pxDelayedTaskList );
      xNextTaskUnblockTime = listGET_LIST_ITEM_VALUE( &( ( pxTCB )->xStateListItem ) );
    }
}

TaskHandle_t xTaskGetCurrentTaskHandle( void )
{
  TaskHandle_t xReturn;
  /* A critical section is not required as this is not called from
     an interrupt and the current TCB will always be the same for any
     individual execution thread. */
  xReturn = pxCurrentTCB;
  return xReturn;
}

BaseType_t xTaskGetSchedulerState( void )
{
  BaseType_t xReturn;
  if( xSchedulerRunning == pdFALSE )
    xReturn = taskSCHEDULER_NOT_STARTED;
  else
    if( uxSchedulerSuspended == ( UBaseType_t ) pdFALSE )
      xReturn = taskSCHEDULER_RUNNING;
    else
      xReturn = taskSCHEDULER_SUSPENDED;
  return xReturn;
}

TickType_t uxTaskResetEventItemValue( void )
{
  TickType_t uxReturn;
  uxReturn = listGET_LIST_ITEM_VALUE( &( pxCurrentTCB->xEventListItem ) );
  /* Reset the event list item to its normal value - so it can be used with
     queues and semaphores. */
  listSET_LIST_ITEM_VALUE( &( pxCurrentTCB->xEventListItem ), ( ( TickType_t ) configMAX_PRIORITIES - ( TickType_t ) pxCurrentTCB->uxPriority ) );
  return uxReturn;
}

static void prvAddCurrentTaskToDelayedList( TickType_t xTicksToWait, const BaseType_t xCanBlockIndefinitely )
{
  TickType_t xTimeToWake;
  const TickType_t xConstTickCount = xTickCount;
  /* Remove the task from the ready list before adding it to the blocked list
     as the same list item is used for both lists. */
  if( uxListRemove( &( pxCurrentTCB->xStateListItem ) ) == ( UBaseType_t ) 0 )
    /* The current task must be in a ready list, so there is no need to
       check, and the port reset macro can be called directly. */
    portRESET_READY_PRIORITY( pxCurrentTCB->uxPriority, uxTopReadyPriority );
  else
    mtCOVERAGE_TEST_MARKER();
  if( ( xTicksToWait == portMAX_DELAY ) && ( xCanBlockIndefinitely != pdFALSE ) )
    /* Add the task to the suspended task list instead of a delayed task
       list to ensure it is not woken by a timing event.  It will block
       indefinitely. */
    vListInsertEnd( &xSuspendedTaskList, &( pxCurrentTCB->xStateListItem ) );
  else
    {
      /* Calculate the time at which the task should be woken if the event
         does not occur.  This may overflow but this doesn't matter, the
         kernel will manage it correctly. */
      xTimeToWake = xConstTickCount + xTicksToWait;
      /* The list item will be inserted in wake time order. */
      listSET_LIST_ITEM_VALUE( &( pxCurrentTCB->xStateListItem ), xTimeToWake );
      if( xTimeToWake < xConstTickCount )
        /* Wake time has overflowed.  Place this item in the overflow
           list. */
        vListInsert( pxOverflowDelayedTaskList, &( pxCurrentTCB->xStateListItem ) );
      else
        {
          /* The wake time has not overflowed, so the current block list
             is used. */
          vListInsert( pxDelayedTaskList, &( pxCurrentTCB->xStateListItem ) );
          /* If the task entering the blocked state was placed at the
             head of the list of blocked tasks then xNextTaskUnblockTime
             needs to be updated too. */
          if( xTimeToWake < xNextTaskUnblockTime )
            xNextTaskUnblockTime = xTimeToWake;
          else
            mtCOVERAGE_TEST_MARKER();
        }
    }
}
