;; Copyright (c) 2010, Álvaro Fernández Díaz
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     ;; Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     ;; Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     ;; Neither the name of the copyright holders nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; @author Álvaro Fernández Díaz
;; @copyright 2010 Álvaro Fernández Díaz
;; @doc
;; @private



;; McErlang Mode

(require 'tempo)


(defun mcerl-erlang-mode-hook ()
  (mcerl-mode-init)
  )


(defun mcerl-mode-init ()
  "Initialize the mcerlmode"
  (require 'tempo)
  (setq debug-on-error t)
  (setq tempo-interactive t)
  (setq max-lisp-eval-depth 500)
  (unless (boundp 'mcerl-max-menu-length)
    (setq mcerl-max-menu-length 30))

  (modify-frame-parameters (selected-frame)
     '((menu-bar-lines . 20)))
  (display-menu)
;  (mcerl-mode-keymap-init)
;  (expand-menu mcerl-menu-items)
;  (mcerl-menu-init "McErlang" mcerl-menu-items (make-sparse-keymap "McErlang"))
)


;; Displays McErlang Menu
(defun display-menu ()
 
  (define-key-after global-map [menu-bar mcerlang]
    (cons "McErlang" (make-sparse-keymap "McErlang Utilities") ))
  
;; First Level Tabs

(define-key global-map [menu-bar mcerlang Miscelanea]
    (cons "Miscelanea" (make-sparse-keymap "Various McErlang Statements")))



  (define-key global-map [menu-bar mcerlang structure]
    (cons "Sample Structures" (make-sparse-keymap "McErlang Structure Skeletons")))
  
(define-key global-map [menu-bar mcerlang Functions]
    (cons "Editing McErlang" (make-sparse-keymap "Functions to interact with McErlang features")))


  (define-key global-map [menu-bar mcerlang Functions globald]
    (cons "Global Dictionary" (make-sparse-keymap "Functions to access McErlang Global Dictionary")))
  

  (define-key global-map [menu-bar mcerlang Functions noded]
    (cons "Node Dictionary" (make-sparse-keymap "Functions to access McErlang Node Dictionary")))
  
  (define-key global-map [menu-bar  mcerlang Functions probe-actions]
    (cons "Probe Actions" (make-sparse-keymap "Functions to handle McErlang Probe Actions")))
  
 (define-key global-map [menu-bar  mcerlang Functions probe-states]
    (cons "Probe States" (make-sparse-keymap "Functions to handle McErlang Probe States")))
  
  (define-key global-map [menu-bar mcerlang Functions LTL2Buchi]
    (cons "LTL2Büchi" (make-sparse-keymap "Functions to generate Büchi automatons from LTL formulae")))

  (define-key global-map [menu-bar mcerlang Functions LTL]
    (cons "LTL" (make-sparse-keymap "Functions to generate LTL formulae")))



  (define-key global-map [menu-bar mcerlang Functions Utils]
    (cons "Utils" (make-sparse-keymap "Useful functions to use during McErlang editing")))

  (define-key global-map [menu-bar mcerlang Functions Utils RecState]
    (cons "Record State" (make-sparse-keymap "Record State")))


 (define-key global-map [menu-bar mcerlang Functions Utils RecNode]
    (cons "Record Node" (make-sparse-keymap "Record Node")))


  (define-key global-map [menu-bar mcerlang Functions Utils RecProcess]
    (cons "Record Process" (make-sparse-keymap "Record Process")))


  (define-key global-map [menu-bar mcerlang Functions Utils RecProcessFlags]
    (cons "Record ProcessFlags" (make-sparse-keymap "Record Process Flags")))




 (define-key global-map [menu-bar mcerlang McErlangShell]
    (cons "McErlang Shell" (make-sparse-keymap "Functions to create and interact with a McErlang shell")))
 
(define-key global-map [menu-bar mcerlang McErlangShell Options]
    (cons "Model Checking Options" (make-sparse-keymap "Options Configuration for Model Checking")))



(define-key global-map [menu-bar mcerlang Debugger]
    (cons "Debugger" (make-sparse-keymap "Functions to start and interact with McErlang Debugger")))



;; Second Level Tabs

(define-key global-map
  [menu-bar mcerlang structure scheduler]
  '("Scheduler: Forbid Sends" "Creates a new buffer with a sample McErlang scheduler structure" .  create-scheduler))

  
  (define-key global-map
  [menu-bar mcerlang structure safety-monitor]
  '("Safety Monitor: Sample" "Creates a new buffer with a McErlang safety monitor structure" .  create-safety-monitor))


  (define-key global-map
    [menu-bar mcerlang structure deadlock-monitor]
    '("Safety Monitor: Deadlock" "Creates a new buffer with a McErlang deadlock safety monitor" . create-deadlock-monitor))

  (define-key global-map 
    [menu-bar mcerlang structure implication-monitor] 
    '("Buchi Monitor: ¬[](P->Q) " "Creates a new buffer with a McErlang buchi monitor for LTL expression \"not always (P implies Q)\"" . create-buchi-implication-monitor))

  (define-key global-map 
    [menu-bar mcerlang structure buchi-monitor] 
    '("Buchi Monitor: Sample" "Creates a new buffer with a McErlang buchi monitor sample" . create-buchi-monitor))

;Miscelanea			
  
  (define-key global-map
    [menu-bar mcerlang  Miscelanea insert-choice]
    '("Choice Statement " "Generates several execution alternatives." . tempo-template-insert-choice-statement ))

 (define-key global-map
    [menu-bar mcerlang Miscelanea cancel-transformations]
    '("Language" "Cancels transformations from this module into McErlang" . tempo-template-cancel-transformations ))


 

;Functions 

  (define-key global-map
    [menu-bar mcerlang Functions noded insert-nget0]
    '("get/0 " "Obtains the contents of the Node Dictionary" . tempo-template-insert-nget0 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions noded insert-nerase2]
    '("nerase/0" "Deletes a key/value pair from the Node Dictionary" . tempo-template-insert-nerase2 ))
  
  
  (define-key global-map
    [menu-bar mcerlang Functions noded insert-nget1]
  '("get/1 " "Obtains the value associated to a key from the Node Dictionary" . tempo-template-insert-nget1 ))
  
  (define-key global-map
  [menu-bar mcerlang Functions noded insert-nput2]
  '("nput/2" "Insert a key/value pair in the Node Dictionary" . tempo-template-insert-nput2 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions noded insert-nerase0]
    '("nerase/0" "Deletes all the contents from the Node Dictionary" . tempo-template-insert-nerase0 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions globald insert-gget0]
    '("gget/0 " "Obtains the contents of the Global Dictionary" . tempo-template-insert-gget0 ))
  
  (define-key global-map 
    [menu-bar mcerlang Functions globald insert-gget1]
    '("gget/1 " "Obtains the value associated to a key from the Global Dictionary" . tempo-template-insert-gget1 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions globald insert-gput2]
    '("gput/2" "Insert a key/value pair in the Global Dictionary" . tempo-template-insert-gput2 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions globald insert-gerase0]
    '("gerase/0" "Deletes all the contents from the Global Dictionary" . tempo-template-insert-gerase0 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions globald insert-gerase2]
    '("gerase/2" "Deletes a key/value pair from the Global Dictionary" . tempo-template-insert-gerase2 ))
  
 
  
  (define-key global-map
    [menu-bar mcerlang Functions probe-states probe-state1]
    '("probe_state/1" "Introduces a persistent probe state (Label/void pair) in the Global Dictionary of current State" . tempo-template-insert-probe-state1 ))
  
  (define-key global-map
    [menu-bar mcerlang Functions probe-states probe-state2]
    '("probe_state/2" "Introduces a persistent probe state (Label/Value pair) in the Global Dictionary of current State" . tempo-template-insert-probe-state2 ))
  
   (define-key global-map
     [menu-bar mcerlang Functions probe-states del-probe-state]
     '("del_probe_state/1" "Deletes a persistent probe state (Label/Value pair) from the Global Dictionary of current State" . tempo-template-insert-del-probe-state ))
   
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-states get-probe-state1]
     '("get_probe_state/1" "Gets the probe state, identified by Label parameter, from the Global Dictionary of current State" . tempo-template-insert-get-probe-state1 ))
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-states get-probe-state2]
     '("get_probe_state/2" "Gets the probe state, identified by Label parameter, from the Global Dictionary of parameter State" . tempo-template-insert-get-probe-state2 ))
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-states has-probe-state]
     '("has_probe_state/2" "Checks whether a probe state (Label/Value pair) for parameter Label exists in the Global Dictionary of parameter State" . tempo-template-insert-has-probe-state))
  
   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-probe1]
     '("probe/1" "Inserts a probe action (Label/void pair) for parameter Label in the Transition Stack" . tempo-template-insert-probe1))
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-probe2]
     '("probe/2" "Inserts a probe action (Label/Value pair) for parameters Label and Value in the Transition Stack" . tempo-template-insert-probe2))
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-is-probe]
     '("is_probe/1" "Checks whether parameter Action is a probe action" . tempo-template-insert-is-probe))
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-probe-term]
     '("probe_term/1" "Obtains the Value term for parameter Action, whichs shall be a probe action" . tempo-template-insert-probe-term))


   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-probe-label]
     '("probe_label/1" "Obtains the Label for parameter Action, whichs shall be a probe action" . tempo-template-insert-probe-label))

   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-match-probe-label]
     '("match_probe_label/1" "If parameter Action is a probe action, return its Label or \"false\" otherwise" . tempo-template-insert-match-probe-label))
   
   (define-key global-map
     [menu-bar mcerlang Functions probe-actions insert-has-probe-action]
     '("Generate has_probe_action/1" "Generates a function that checks whether there is a probe action in a list of actions and returns \"{true, Label}\" or \"no\" if there is not" . tempo-template-insert-has-probe-action))
 
; LTL2BUCHI

(define-key global-map
  [menu-bar mcerlang Functions LTL2Buchi ltl2module-load]
  '("ltl2module_and_load/2" "Generates a Büchi automaton from a LTL formula expressed using McErlang \"ltl\" module. Returns a reference to the temporary location of that automaton." . tempo-template-ltl2buchi-ltl2module-load))

(define-key global-map
  [menu-bar mcerlang Functions LTL2Buchi ltl2module]
  '("ltl2module/2" "Generates a Büchi automaton from a LTL formula expressed using McErlang \"ltl\" module and stores it in the specified file." . tempo-template-ltl2buchi-ltl2module))


(define-key global-map
  [menu-bar mcerlang Functions LTL2Buchi string2module-load]
  '("ltl_string2module_and_load/2" "Generates a Büchi automaton from a LTL formula parsed from a String. Returns a reference to the temporary location of that automaton." . tempo-template-ltl2buchi-string2module-load))

(define-key global-map
  [menu-bar mcerlang Functions LTL2Buchi string2module]
  '("ltl_string2module/2" "Generates a Büchi automaton from a LTL formula parsed from a String and stores it in the specified file." . tempo-template-ltl2buchi-string2module))

(define-key global-map
  [menu-bar mcerlang Functions LTL2Buchi string]
  '("string/1" "Parses a LTL formula from a String" . tempo-template-ltl2buchi-string))


;; LTL
  
(define-key global-map
     [menu-bar mcerlang Functions LTL until]
     '("until/2" "Introduces a LTL Until (U) expression" . tempo-template-ltl-until))

(define-key global-map
     [menu-bar mcerlang Functions LTL release]
     '("release/2" "Introduces a LTL Release (R) expression" . tempo-template-ltl-release))

 
  (define-key global-map
     [menu-bar mcerlang Functions LTL prop]
     '("prop/1" "Introduces a proposition " . tempo-template-ltl-prop))

(define-key global-map
     [menu-bar mcerlang Functions LTL next]
     '("next/1" "Introduces a LTL Next (X) expression" . tempo-template-ltl-next))

(define-key global-map
  [menu-bar mcerlang Functions LTL ltrue]
  '("ltrue/0" "Generates \"True\" constant" . tempo-template-ltl-ltrue))

(define-key global-map
  [menu-bar mcerlang Functions LTL lor]
  '("lor/2" "Introduces a LTL Or (||) expression" . tempo-template-ltl-lor))


(define-key global-map
  [menu-bar mcerlang Functions LTL lnot]
  '("lnot/1" "Introduces a LTL Not (¬) expression" . tempo-template-ltl-lnot))
  
(define-key global-map
  [menu-bar mcerlang Functions LTL lfalse]
  '("lfalse/0" "Generates \"False\" constant" . tempo-template-ltl-lfalse))

(define-key global-map
  [menu-bar mcerlang Functions LTL land]
  '("land/2" "Introduces a LTL And (&&) expression" . tempo-template-ltl-land))

(define-key global-map
  [menu-bar mcerlang Functions LTL implication]
  '("implication/2" "Introduces a LTL Implication (->) expression" . tempo-template-ltl-implication))

(define-key global-map
  [menu-bar mcerlang Functions LTL eventually]
  '("eventually/1" "Introduces a LTL Eventually (<>) expression" . tempo-template-ltl-eventually))

 (define-key global-map
   [menu-bar mcerlang Functions LTL equivalent]
   '("equivalent/2" "Introduces a predicate (==) that is true if both its parameter predicates have the same truth value" . tempo-template-ltl-equivalent))


(define-key global-map
  [menu-bar mcerlang Functions LTL always]
  '("always/1" "Introduces a LTL Always ([]) expression" . tempo-template-ltl-always))


;Debugger
 

   (define-key global-map
     [menu-bar mcerlang Debugger stop_debugger]
     '("Stop Debugger" "Stop McErlang Debugger and return to Erlang shell" . stop-debugger)   
     )

  (define-key global-map
     [menu-bar mcerlang Debugger run-debugger]
     '("Run" "Runs program until it halts." . run-debugger)   
     )


  (define-key global-map
     [menu-bar mcerlang Debugger print-transitions-debugger]
     '("Print Transitions" "Print transitions enabled from current state" . print-transitions-debugger)   
     )


  (define-key global-map
     [menu-bar mcerlang Debugger print-state-debugger]
     '("Print State" "Print all information about current state" . print-state-debugger)   
     )

  (define-key global-map
     [menu-bar mcerlang Debugger print-ether-debugger]
     '("Print Ether" "Print sent messages not yet in its destiny mailbox in current state" . print-ether-debugger)   
     )

  (define-key global-map
     [menu-bar mcerlang Debugger print-node-names-debugger]
     '("Print Node Names" "Print the name of all nodes running in current state" . print-node-names-debugger)   
     )

  (define-key global-map
     [menu-bar mcerlang Debugger print-process-names-debugger]
     '("Print Process Names" "Print the name of all process running in current state" . print-process-names-debugger)   
     )


  (define-key global-map
     [menu-bar mcerlang Debugger print-node-debugger]
     '("Print Node" "Print the information about certain node in current state" . print-node-debugger)   
     )

  (define-key global-map
     [menu-bar mcerlang Debugger print-process-debugger]
     '("Print Process" "Print the information about certain process in current state" . print-process-debugger)   
     )


  (define-key global-map
     [menu-bar mcerlang Debugger where-debugger]
     '("Where/0" "Show current context." . where-debugger)   
     )
  (define-key global-map
     [menu-bar mcerlang Debugger whereN-debugger]
     '("Where/1" "Show last N stack frames." . whereN-debugger)   
     )

  ;; (define-key global-map
  ;;    [menu-bar mcerlang Debugger step-debugger]
  ;;    '("Step/0" "Takes a single computation step." . step-debugger)   
  ;;    )

  ;; (define-key global-map
  ;;    [menu-bar mcerlang Debugger stepN-debugger]
  ;;    '("Step/1" "Takes N computation steps." . stepN-debugger)   
  ;;    )


  (define-key global-map
     [menu-bar mcerlang Debugger forward-debugger]
     '("Forward/0" "Move stack pointer one step towards the end of the program." . forward-debugger)  
     ) 

  (define-key global-map
     [menu-bar mcerlang Debugger forwardN-debugger]
     '("Forward/1" "Move stack pointer N steps towards the end of the program." . forwardN-debugger)   
     )

   (define-key global-map
     [menu-bar mcerlang Debugger back-debugger]
     '("Back/0" "Go to previous state of current execution" . back-debugger)   
     )

   (define-key global-map
     [menu-bar mcerlang Debugger backN-debugger]
     '("Back/1" "Go to nth previous state of current execution" . backN-debugger)   
     )


  (define-key global-map
     [menu-bar mcerlang Debugger whereN-debugger]
     '("Where/1" "Show last N stack frames." . whereN-debugger)   
     )



  (define-key global-map
     [menu-bar mcerlang Debugger run-debugger]
     '("Run" "Runs program until it halts." . run-debugger)   
     )

 (define-key global-map
     [menu-bar mcerlang Debugger goto-debugger]
     '("Go To Transition" "Go to certain transition" . goto-debugger)   
  )

  (define-key global-map
     [menu-bar mcerlang Debugger goto-start-debugger]
     '("Go to start" "Go to the initial state of the execution" . goto-start-debugger)   
     )


   (define-key global-map
     [menu-bar mcerlang Debugger show-execution-debugger]
     '("Show Execution" "Show Execution Trace from initial state to current state" . show-execution-debugger)   
  )


  (define-key global-map
     [menu-bar mcerlang Debugger start_debugger]
     '("Start Debugger" "Start McErlang Debugger over last execution" . start-debugger)   
     )

; McErlang Utils


(define-key global-map
     [menu-bar mcerlang Functions Utils LTL-parametrical-function]
     '("propositional-function/1" "Generates a function that can be used as parameter for a LTL propositional formula." . tempo-template-LTL-parametrical-function)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils all-processes]
     '("allProcesses/1" "Obtains a list of all processes contained in parameter system state" . tempo-template-all-processes)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils do]
     '("do/3" "Do loop" . tempo-template-utils-do)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils find]
     '("find/3" "Function to find certain element of a list" . tempo-template-utils-find)   
     )


  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode name]
     '("name" "Represents the name of an Erlang node" . tempo-template-node-name)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode processes]
     '("processes" "List of processes running in an Erlang node" . tempo-template-node-processes)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode registered]
     '("registered" "Registered processes. [{Pid, name}] " . tempo-template-node-registered)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode monitor]
     '("monitor" "Enabled process monitors" . tempo-template-node-monitor)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode nodeMonitors]
     '("node_monitors" "Enabled node monitors " . tempo-template-node-nodeMonitors)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode dict]
     '("dict" "Node dictionary" . tempo-template-node-dict)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecNode links]
     '("links" "List of links " . tempo-template-node-links)   
     )

  (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcessFlags trapExit]
     '("trap_exit" "Value of trap_exit flag" . tempo-template-ProcessFlags-trapExit)   
     )

 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcessFlags doTerminate]
     '("do_terminate" "Value of do_terminate flag" . tempo-template-ProcessFlags-doTerminate)   
     )


 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcess flags]
     '("flags" "Process flags. Record ProcessFlags" . tempo-template-process-flags)   
     )

 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcess dict]
     '("dict" "Process dictionary" . tempo-template-process-dict)   
     )


 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcess queue]
     '("queue" "Process mailbox. [term()]" . tempo-template-process-queue)   
     )

 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcess pid]
     '("pid" "Process identifier" . tempo-template-process-pid)   
     )

 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcess expr]
     '("expr" "..." . tempo-template-process-expr)   
     )

 (define-key global-map
     [menu-bar mcerlang Functions Utils RecProcess status]
     '("status" "Process status (blocked | receivable)" . tempo-template-process-status)   
     )

(define-key global-map
     [menu-bar mcerlang Functions Utils RecState ether]
     '("ether" "Messages in transit. [{From, To, Message}]" . tempo-template-state-ether)   
     )

(define-key global-map
     [menu-bar mcerlang Functions Utils RecState nodes]
     '("nodes" "List of nodes in the system" . tempo-template-state-nodes)   
     )

(define-key global-map
     [menu-bar mcerlang Functions Utils RecState dict]
     '("dict" "System global dictionary" . tempo-template-state-dict)   
     )


 
;McErlangShell

(define-key global-map
  [menu-bar mcerlang McErlangShell compile]
  '("Compile All" "Compiles all Erlang files in current directory" . compile)   
  )


(define-key global-map
  [menu-bar mcerlang McErlangShell compile-file]
  '("Compile File" "Compiles and loads specified Erlang file under McErlang with default options." . compile-file)   
  )


(define-key global-map
  [menu-bar mcerlang McErlangShell compile-file-options]
  '("Compile File + Options" "Compiles and loads specified Erlang file under McErlang with specified options." . compile-file-options)   
  )
  

(define-key global-map
     [menu-bar mcerlang McErlangShell mcerl-start]
     '("Start Run" "Generates Command to Start a Model Checking Run" . tempo-template-mce-start-program)   
  )

  
(define-key global-map
  [menu-bar mcerlang McErlangShell mce_opts]
  '("Load mce_opts" "Load mce_opts record definition." . load-mce_opts)   
  )



(define-key global-map
  [menu-bar mcerlang McErlangShell myshell]
  '("Start McErlang Shell" "Starts an interactive McErlang shell" . mymcerl)   
  )


;; MC OPTIONS

(define-key global-map [menu-bar mcerlang McErlangShell Options Shortest]
    (cons "Shortest" (make-sparse-keymap "Shortest Path")))

(define-key global-map [menu-bar mcerlang McErlangShell Options Monitor]
    (cons "Monitor" (make-sparse-keymap "Monitor for Model Checking")))







(define-key global-map [menu-bar mcerlang McErlangShell Options Algorithm]
    (cons "Algorithm" (make-sparse-keymap "Algorithm for Model Checking")))



(define-key global-map
  [menu-bar mcerlang McErlangShell Options Algorithm mce-algorithm-safety]
  '("mce_alg_safety" "Runs specified monitor, of type safety, in every state of the program. " . tempo-template-mce-algorithm-safety)   
  )


(define-key global-map
  [menu-bar mcerlang McErlangShell Options Algorithm mce-algorithm-simulation]
  '("mce_alg_simulation" "Executes a single run of the program executing a monitor, of type safety, in every generated state. " . tempo-template-mce-algorithm-simulation)   
  )


(define-key global-map
  [menu-bar mcerlang McErlangShell Options Algorithm mce-algorithm-debugger]
  '("mce_alg_debugger" "Runs McErlang Debugger on the specified program. " . tempo-template-mce-algorithm-debugger)   
  )

(define-key global-map
  [menu-bar mcerlang McErlangShell Options Algorithm mce-algorithm-buechi]
  '("mce_alg_buechi" "Runs the specified monitor, of type buechi, over the generated state space. " . tempo-template-mce-algorithm-buechi)   
  )


(define-key global-map
  [menu-bar mcerlang McErlangShell Options Algorithm mce-algorithm-safety-parallel]
  '("mce_alg_safety_parallel" "Runs specified monitor, of type safety, in every state of the program, possibly distributing that computation among different processors. " . tempo-template-mce-algorithm-safety-parallel)   
  )

(define-key global-map
  [menu-bar mcerlang McErlangShell Options Algorithm mce-algorithm-combine]
  '("mce_alg_combine" "Allows the use of different Model Checking Algorithms, specified as parameter. Use of custom schedulers recommended. " . tempo-template-mce-algorithm-combine)   
  )

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Algorithm mcerl-algorithm]
     '("Insert Algorithm" "Specifies Model Checking Algorithm to be used." . tempo-template-mce-algorithm)   
  )


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Monitor mcerl-monitor-deadlock]
     '("Deadlock Monitor" "Fails if all processes are blocked." . tempo-template-mce-monitor-deadlock)   
)


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Monitor mcerl-monitor-maxqueue]
     '("Max Queue Size  Monitor" "Fails if any queue has more messages than the ones specified by parameter." . tempo-template-mce-monitor-maxqueue)   
)


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Monitor mcerl-monitor]
     '("Insert Any Monitor" "Specifies Monitor to be used during Model Checking" . tempo-template-mce-monitor)   
)


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Shortest shortest-true]
     '("True (default)" "Compute Shortest Path to counterexample." . tempo-template-mce-shortest-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Shortest shortest-false]
     '("False" "Do not compute Shortest Path to counterexample." . tempo-template-mce-shortest-false)   
)

;**NEW ONES

(define-key global-map [menu-bar mcerlang McErlangShell Options Transitions]
    (cons "Transitions" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Transitions transitions]
     '("Transitions" "Function for computing next transition" . tempo-template-mce-transitions)   
)



(define-key global-map [menu-bar mcerlang McErlangShell Options Commit]
    (cons "Commit" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Commit commit]
     '("Commit" "Function for commiting to transition" . tempo-template-mce-commit)   
)




(define-key global-map [menu-bar mcerlang McErlangShell Options Sim_external_world]
    (cons "Sim_external_world" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Sim_external_world true]
     '("True" "Allow interaction with external world (read/write files ...) during simulation" . tempo-template-mce-sim_external_world-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Sim_external_world false]
     '("False (default)" "Do not simulate interaction with external world (read/write files ...)" . tempo-template-mce-sim_external_world-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Random]
    (cons "Random" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Random true]
     '("True" "Randomize the order of transitions" . tempo-template-mce-random-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Random false]
     '("False (default)" "Order of transitions defined by transition computation function" . tempo-template-mce-random-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options PathLimit]
    (cons "PathLimit" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options PathLimit]
     '("PathLimit" "Set a maximum depth for search paths" . tempo-template-mce-pathLimit)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Terminate]
    (cons "Terminate" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Terminate true]
     '("True" "TODO: Not defined" . tempo-template-mce-terminate-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Terminate false]
     '("False (default)" "TODO: Not defined" . tempo-template-mce-terminate-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Is_simulation]
    (cons "Is_simulation" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Is_simulation true]
     '("True" "TODO: Not defined" . tempo-template-mce-is_simulation-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Is_simulation false]
     '("False (default)" "TODO: Not defined" . tempo-template-mce-is_simulation-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Small_pids]
    (cons "Small_pids" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Small_pids true]
     '("True" "Try to reuse pids when possible" . tempo-template-mce-small_pids-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Small_pids false]
     '("False (default)" "Use a different new Pid for every process" . tempo-template-mce-small_pids-false)   
)

(define-key global-map [menu-bar mcerlang McErlangShell Options Notice_exits]
    (cons "Notice_exits" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Notice_exits true]
     '("True (default)" "Show a warning message in standard output when a process crashes." . tempo-template-mce-notice_exits-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Notice_exits false]
     '("False" "Do print a warning message when a process crashes" . tempo-template-mce-notice_exits-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Notice_fail_on_exit]
    (cons "Fail_on_exit" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Notice_fail_on_exit true]
     '("True (default)" "Fail if user code crashes (not exit signals sent to linked processes)" . tempo-template-mce-fail_on_exit-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Notice_fail_on_exit false]
     '("False" "Do not fail as soon as user code crashes (sends )" . tempo-template-mce-fail_on_exit-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Sim_actions]
    (cons "Sim_actions" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Sim_actions true]
     '("True" "TODO: Print actions during simulation?" . tempo-template-mce-sim_actions-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Sim_actions false]
     '("False(default)" "TODO: Do not print actions during simulation.?" . tempo-template-mce-sim_actions-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Output]
    (cons "Output" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Output true]
     '("True" "TODO: Enable output from mcerlang:format" . tempo-template-mce-output-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Output false]
     '("False(default)" "TODO: Disable output from mcerlang:format" . tempo-template-mce-output-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Sim_keep_stack]
    (cons "Sim_keep_stack" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Sim_keep_stack true]
     '("True(default)" "Keep execution stack during simulation" . tempo-template-mce-sim_keep_stack-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Sim_keep_stack false]
     '("False" "Do not keep execution stack during simulation" . tempo-template-mce-sim_keep_stack-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Start_debugger]
    (cons "Start_debugger" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Start_debugger true]
     '("True" "Start debugger directly upon failure" . tempo-template-mce-start_debugger-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Start_debugger false]
     '("False(default)" "Do not start debugger directly upon failure" . tempo-template-mce-start_debugger-false)   
)



(define-key global-map [menu-bar mcerlang McErlangShell Options Save_result]
    (cons "Save_result" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Save_result true]
     '("True(default)" "Save result of McErlang run" . tempo-template-mce-save_result-true)   
)


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Save_result false]
     '("False" "Do not save result of McErlang run" . tempo-template-mce-save_result-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Language]
    (cons "Language" (make-sparse-keymap "Options Configuration for Model Checking")))


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Language erlang]
     '("Erlang(default)" "Language of verified programs is Erlang" . tempo-template-mce-language-erlang)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Language other]
     '("Other" "Set language of verified programs" . tempo-template-mce-language-other)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Distributed_semantics]
    (cons "Distributed_semantics" (make-sparse-keymap "Options Configuration for Model Checking")))

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Distributed_semantics true]
     '("True" "Process in the same node use semantics for nodes in different nodes" . tempo-template-mce-distributed_semantics-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Distributed_semantics false]
     '("False(default)" "Process in the same node use semantics for nodes in same node" . tempo-template-mce-distributed_semantics-false)   
)


(define-key global-map [menu-bar mcerlang McErlangShell Options Chatter]
    (cons "Chatter" (make-sparse-keymap "Options Configuration for Model Checking"))
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Chatter normal]
     '("Normal" "TODO: Level of verbosity--What does it print?" . tempo-template-mce-chatter-normal)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Chatter verbose]
     '("Verbose" "TODO: Level of verbosity--What does it print?" . tempo-template-mce-chatter-verbose)   
)


(define-key global-map
     [menu-bar mcerlang McErlangShell Options Time_limit]
     '("Time_limit" "Set a maximum number of seconds of verification" . tempo-template-mce-time_limit)   
)

(define-key global-map [menu-bar mcerlang McErlangShell Options Is_infinitely_fast]
    (cons "Is_infinitely_fast" (make-sparse-keymap "Options Configuration for Model Checking"))
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Is_infinitely_fast true]
     '("True" "Only execute \"After clauses\" of a receive if there are no messages in the process mailbox" . tempo-template-mce-is_infinitely_fast-true)   
)

(define-key global-map
     [menu-bar mcerlang McErlangShell Options Is_infinitely_fast false]
     '("False(default)" "Execute \"After clauses\" of a receive whenever possible" . tempo-template-mce-is_infinitely_fast-false)   
)


)



; END display-menu()
   
   


;;SIMPLE STATEMENTS



(tempo-define-template "insert-probe-action"
 '("mce_erl:probe( " (p "Label: ") " ," (p "Term: ") " )" > n)
)

;;* Choice



(tempo-define-template "insert-choice-statement"
 '("mce_erl:choice([{ }])" > n )
)


;;* nGet/0



(tempo-define-template "insert-nget0"
 '("mcerlang:nget()" > n )
)

;* nGet/1

(tempo-define-template "insert-nget1" 
 '("mcerlang:nget(" (p "Key: ") ")" > n )
)

;* nput/2


(tempo-define-template "insert-nput2" 
 '("mcerlang:nput(" (p "Key: ") "," (p "Value: ") ")" > n )
)


;* nerase/0

(tempo-define-template "insert-nerase0" 

 '("mcerlang:nerase()." > n )
)

;* nerase/2



(tempo-define-template "insert-nerase2" 
 '("mcerlang:nerase(" (p "Key: ") "," (p "Value: ") ")" > n )
)


;;* gGet/0



(tempo-define-template "insert-gget0"
 '("mcerlang:gget()" > n )
)

;* gGet/1

(tempo-define-template "insert-gget1" 
 '("mcerlang:gget(" (p "Key: ") ")" > n )
)

;* gput/2

(tempo-define-template "insert-gput2" 
 '("mcerlang:gput(" (p "Key: ") "," (p "Value: ") ")" > n )
)


;* gerase/0

(tempo-define-template "insert-gerase0" 
 '("mcerlang:gerase()" > n )
)

;* gerase/2

(tempo-define-template "insert-gerase2" 
 '("mcerlang:gerase(" (p "Key: ") "," (p "Value: ") ")" > n )
)


;* -language(erlang)

(tempo-define-template "cancel-transformations" 
 '("-language(erlang)" > n )
)


;* probeState1

(tempo-define-template "insert-probe-state1" 
 '("mce_erl:probe_state(" (p "Label: ") ")" > n )
)


;* probeState2

(tempo-define-template "insert-probe-state2" 
 '("mce_erl:probe_state(" (p "Label: ") ", " (p "Value ") ")" > n )
)


;* del_probeState

(tempo-define-template "insert-del-probe-state" 
 '("mce_erl:del_probe_state(" (p "Label: ") ")" > n )
)


;* get_probeState1

(tempo-define-template "insert-get-probe-state1" 
 '("mce_erl:get_probe_state(" (p "Label: ") ")" > n )
)

;* has_probeState

(tempo-define-template "insert-has-probe-state" 
 '("mce_erl:has_probe_state(" (p "Label: ") ", " (p "State: ") ")" > n )
)

;* get_probeState2

(tempo-define-template "insert-get-probe-state2" 
 '("mce_erl:get_probe_state(" (p "Label: ") ", " (p "State: ") ")" > n )
)


;* probe1

(tempo-define-template "insert-probe1" 
 '("mce_erl:probe(" (p "Label: ") ")" > n )
)

;* probe2

(tempo-define-template "insert-probe2" 
 '("mce_erl:probe(" (p "Label: ") ", "(p "Value: ") ")" > n )
)


;* isprobe

(tempo-define-template "insert-is-probe" 
 '("mce_erl:is_probe(" (p "Action: ") ")" > n )
)

;* probe_label

(tempo-define-template "insert-probe-label" 
 '("mce_erl:probe_label(" (p "Action: ") ")" > n )
)

;* probe_term

(tempo-define-template "insert-probe-term" 
 '("mce_erl_actions:get_probe_term(" (p "Action: ") ")" > n )
)


;* match_probe_label

(tempo-define-template "insert-match-probe-label" 
 '("mce_erl:match_probe_label(" (p "Action: ") ")" > n )
)

;* has_probe_action

(tempo-define-template "insert-has-probe-action" 
 '("has_probe_action( Actions)->" > n
   "mce_utils:findret(fun mce_erl:match_probe_label/1, Actions)"  > n )
)

;* Load mce_opts.

(tempo-define-template "load-mce_opts" 
 '("rr(mce:find_mce_opts())." n)
)

(defun load-mce_opts ()
  (interactive)
  (tempo-template-load-mce_opts)
  (comint-send-input)
  ;(message "Remember to include file \"mce_opts.hrl\" in current working directory")
)

;* Start Debugger.

(tempo-define-template "start-debugger" 
 '("mce_erl_debugger:start(mce:result()).") 
)

(defun start-debugger ()
  (interactive)
  (tempo-template-start-debugger)
  (comint-send-input)
)


;* Stop Debugger.

(tempo-define-template "stop-debugger" 
 '("quit().") 
)

(defun stop-debugger ()
  (interactive)
  (tempo-template-stop-debugger)
  (comint-send-input)
)


;* Go one debugger step back.

(tempo-define-template "back-debugger" 
 '("back().") 
)

(defun back-debugger ()
  (interactive)
  (tempo-template-back-debugger)
  (comint-send-input)
)

;* Go N debugger steps back.

(tempo-define-template "backN-debugger" 
 '("back("(p "Number of steps back: ") " ).") 
)

(defun backN-debugger ()
  (interactive)
  (tempo-template-backN-debugger)
  (comint-send-input)
)



;* Go to first transition.

(tempo-define-template "goto-start-debugger" 
 '("gotoStart().") 
)

(defun goto-start-debugger ()
  (interactive)
  (tempo-template-goto-start-debugger)
  (comint-send-input)
)

;* Print transitions.

(tempo-define-template "print-transitions-debugger" 
 '("printTransitions().") 
)

(defun print-transitions-debugger ()
  (interactive)
  (tempo-template-print-transitions-debugger)
  (comint-send-input)
)

;* Show execution until current stack step.

(tempo-define-template "show-execution-debugger" 
 '("showExecution().") 
)

(defun show-execution-debugger ()
  (interactive)
  (tempo-template-show-execution-debugger)
  (comint-send-input)
)


;* Go to certain execution step.

(tempo-define-template "goto-debugger" 
 '("goto(" (p "stepNumber: ") ").")
)

(defun goto-debugger ()
  (interactive)
  (tempo-template-goto-debugger)
  (comint-send-input)
)





;* Take certain number of execution steps.

(tempo-define-template "stepN-debugger" 
 '("step(" (p "Number of steps: ") ").")
)

(defun stepN-debugger ()
  (interactive)
  (tempo-template-stepN-debugger)
  (comint-send-input)
)



;* Take one execution step.

(tempo-define-template "step-debugger" 
 '("step().")
)

(defun step-debugger ()
  (interactive)
  (tempo-template-step-debugger)
  (comint-send-input)
)




;* Show context.

(tempo-define-template "where-debugger" 
 '("where().")
)

(defun where-debugger ()
  (interactive)
  (tempo-template-where-debugger)
  (comint-send-input)
)


;* Show context of size N.

(tempo-define-template "whereN-debugger" 
 '("where("(p "Number of last N stack frames: ")" ).")
)

(defun whereN-debugger ()
  (interactive)
  (tempo-template-whereN-debugger)
  (comint-send-input)
)



;* Forward.

(tempo-define-template "forward-debugger" 
 '("forward().")
)

(defun forward-debugger ()
  (interactive)
  (tempo-template-forward-debugger)
  (comint-send-input)
)

;* Forward.

(tempo-define-template "forwardN-debugger" 
 '("forward("(p "Number of steps forward: ") " ).")
)

(defun forwardN-debugger ()
  (interactive)
  (tempo-template-forwardN-debugger)
  (comint-send-input)
)


;* Run.

(tempo-define-template "run-debugger" 
 '("run().")
)

(defun run-debugger ()
  (interactive)
  (tempo-template-run-debugger)
  (comint-send-input)
)

;* Print Functions.

(tempo-define-template "print-state-debugger" 
 '("printState().")
)

(defun print-state-debugger ()
  (interactive)
  (tempo-template-print-state-debugger)
  (comint-send-input)
)



(tempo-define-template "print-ether-debugger" 
 '("printEther().")
)

(defun print-ether-debugger ()
  (interactive)
  (tempo-template-print-ether-debugger)
  (comint-send-input)
)

(tempo-define-template "print-node-names-debugger" 
 '("printNodeNames().")
)

(defun print-node-names-debugger ()
  (interactive)
  (tempo-template-print-node-names-debugger)
  (comint-send-input)
)


(tempo-define-template "print-node-debugger" 
 '("printNode("(p "Node Name: ") " ).")
)

(defun print-node-debugger ()
  (interactive)
  (tempo-template-print-node-debugger)
  (comint-send-input)
)

(tempo-define-template "print-process-debugger" 
 '("printProcess("(p "PID: ") " ).")
)

(defun print-process-names-debugger ()
  (interactive)
  (tempo-template-print-process-names-debugger)
  (comint-send-input)
)

(tempo-define-template "print-process-names-debugger" 
 '("printProcessNames().")
)

(defun print-node-debugger ()
  (interactive)
  (tempo-template-print-node-debugger)
  (comint-send-input)
)

;; LINEAR TEMPORAL LOGIC


(tempo-define-template "ltl-ltrue" 
 '("ltl:ltrue()")
)

(tempo-define-template "ltl-false" 
 '("ltl:lfalse()")
)

(tempo-define-template "ltl-prop" 
 '("ltl:prop(" (p "Proposition ::= fun atom():atom()/int() | {atom(),atom()} | {var,atom()}: ")
    ")")
)

(tempo-define-template "ltl-land" 
 '("ltl:land(" (p "Predicate1: ") ", " (p "Predicate2: ")")")
)

(tempo-define-template "ltl-lor" 
 '("ltl:lor(" (p "Predicate1: ") ", " (p "Predicate2: ")")")
)

(tempo-define-template "ltl-lnot" 
 '("ltl:lnot(" (p "Predicate: ") ")")
)

(tempo-define-template "ltl-implication" 
 '("ltl:implication(" (p "Predicate1: ") ", " (p "Predicate2: ")")")
)

(tempo-define-template "ltl-equivalent" 
 '("ltl:equivalent(" (p "Predicate1: ") ", " (p "Predicate2: ")")")
)

(tempo-define-template "ltl-next" 
 '("ltl:next(" (p "Predicate: ") ")")
)

(tempo-define-template "ltl-always" 
 '("ltl:always(" (p "Predicate: ") ")")
)

(tempo-define-template "ltl-eventually" 
 '("ltl:eventually(" (p "Predicate: ") ")")
)

(tempo-define-template "ltl-until" 
 '("ltl:until(" (p "Predicate1: ") ", " (p "Predicate2: ")")")
)

(tempo-define-template "ltl-release" 
 '("ltl:release(" (p "Predicate1: ") ", " (p "Predicate2: ")")")
)


;; LTL2BUCHI PARSING


(tempo-define-template "ltl2buchi-string" 
 '("mce_ltl_parse:string(\"" (p "Formula (String):  ")"\")")
)

(tempo-define-template "ltl2buchi-string2module" 
 '("mce_ltl_parse:ltl_string2module(\"" (p "Formula (String): ") "\", " (p "File name: ")  ")")
)

(tempo-define-template "ltl2buchi-string2module-load" 
 '("mce_ltl_parse:ltl_string2module_and_load( \"" (p "Formula (String): ") "\", " (p "Monitor name: ")  ")")
)

(tempo-define-template "ltl2buchi-ltl2module" 
 '("mce_ltl_parse:ltl2module(" (p "Formula (LTL): ") ", " (p "File name: ")  ")")
)

(tempo-define-template "ltl2buchi-ltl2module-load" 
 '("mce_ltl_parse:ltl2module_and_load(" (p "Formula (LTL): ") ", " (p "Monitor name: ")  ")")
)


;; Utils



(tempo-define-template "LTL-parametrical-function" 

 '("%%Function that can be invoked as proposition during state space search using algorithm mce_alg_buechi" > n
   "%%ProgramState::state() Current program state" > n
   "%%Actions::[action()] List of actions executed from previous state" > n
   "%%PrivateState::term() Current private state" > n
   "%%Return:  true | false | {true, NewPrivateState}" > n
   (p "Function name: ") "(ProgramState, Actions, PrivateState) ->" > n)
   
)


(tempo-define-template "utils-do" 

 '("%%Function that represents a \"do\" loop" > n
   "%%State::term() Term over which the computations are performed" > n
   "%%CondFun. Boolean function executed over State" > n
   "%%DoFun. Function executed over State in every iteration" > n
   "%%Return: NewState::term()" > n
   "mce_utils:do("(p "State: ")", "(p "CondFun: ")", "(p "DoFun")")" > n)
   
)

(tempo-define-template "utils-find" 

 '("%%Function used to retrieve certain element of a list" > n
   "%%CondFun. Boolean function executed over the elements of List" > n
   "%%List::[term()]" > n
   "%%Return: {ok, Elem} (if Elem is the first element of List that makes CondFun(Elem) return true) | no (otherwise) " > n
   "mce_utils:find("(p "CondFun: ")", "(p "List: ")")" >)
   
)



(tempo-define-template "all-processes" 
 '("mce_erl:allProcesses(" (p "State: ") ")" )
)

(tempo-define-template "node-name" 
 '("#node.name")
)


(tempo-define-template "node-processes" 
 '("#node.processes")
)

(tempo-define-template "node-registered" 
 '("#node.registered")
)

(tempo-define-template "node-monitors" 
 '("#node.monitors")
)

(tempo-define-template "node-nodeMonitors" 
 '("#node.nodeMonitors")
)

(tempo-define-template "node-dict" 
 '("#node.dict")
)

(tempo-define-template "node-links" 
 '("#node.links")
)

(tempo-define-template "ProcessFlags-trapExit" 
 '("#processFlags.trap_exit")
)

(tempo-define-template "ProcessFlags-doTerminate" 
 '("#processFlags.do_terminate")
)

(tempo-define-template "process-status" 
 '("#process.status")
)

(tempo-define-template "process-expr" 
 '("#process.expr")
)

(tempo-define-template "process-pid" 
 '("#process.pid")
)

(tempo-define-template "process-queue" 
 '("#process.queue")
)

(tempo-define-template "process-dict" 
 '("#process.dict")
)

(tempo-define-template "process-flags" 
 '("#process.flags")
)

(tempo-define-template "state-dict" 
 '("#state.dict")
)

(tempo-define-template "state-nodes" 
 '("#state.nodes")
)

(tempo-define-template "state-ether" 
 '("#state.ether")
)


;; END OF SIMPLE STATEMENTS




;; MCE_OPTS ********************************************


;; MONITORS

(tempo-define-template "mce-monitor" 
 '(" , monitor={ " (p "Monitor (default: mce_mon_test, others: mce_mon_deadlock, mce_mon_queue): ") ", "
 (p "Initial argument: " ) "}" n ) 
)


(tempo-define-template "mce-monitor-deadlock" 
 '(" , monitor={mce_mon_deadlock, void}"n ) 
)

(tempo-define-template "mce-monitor-maxqueue" 
 '(" , monitor={mce_mon_queue, void}"n ) 
)





;; ALGORITHMS 
(tempo-define-template "mce-algorithm" 
		       '(" , algorithm={ " (p "Algorithm (default: mce_alg_safety, others: mce_alg_debugger, mce_alg_buechi, mce_alg_safety_parallel, mce_alg_combine): ") ", "
   (p "Initial argument: " ) "}" n ) 
 )


(tempo-define-template "mce-algorithm-safety" 
		       '(" , algorithm={ mce_alg_safety,void} " n ) 
 )


(tempo-define-template "mce-algorithm-simulation" 
		       '(" , algorithm={mce_alg_simulation,void} " n ) 
)

(tempo-define-template "mce-algorithm-debugger" 
		       '(" , algorithm={mce_alg_debugger,void} " n ) 
 )

(tempo-define-template "mce-algorithm-buechi" 
		       '(" , algorithm={mce_alg_buechi,{process_fairness, "(p "Process Fairness: true  | false ")"} " n ) 
		       )

(tempo-define-template "mce-algorithm-safety-parallel" 
		       '(" , algorithm={mce_alg_safety_parallel,"(p "Number of Erlang process schedulers to run: ")"} " n ) 
		       )

(tempo-define-template "mce-algorithm-combine" 
		       '(" , algorithm={mce_alg_combine," > n
			 " {#mce_opts{algorithm= { " (p "First Algorithm: ") ", " 
			 
			 (p "Initial argument: " ) "}" > n n "}}," > n
			 
			 " {#mce_opts{algorithm= { " (p "Second Algorithm: ") ", " 
			 
			 (p "Initial argument: " ) "}" > n n "}}}" > n)
)
;; ABSTRACTIONS
;; TABLE
;; STACK
;; SCHEDULER


 (defun create-scheduler() 
  "Creates a new buffer with a sample McErlang scheduler structure"
  (interactive)
  (let ((new-buf (get-buffer-create "custom_scheduler.erl")) 
	point)    
    (set-buffer new-buf)
    (switch-to-buffer new-buf)
    (erlang-mode)
    (tempo-template-custom-scheduler)
    (message "Buffer custom_scheduler.erl created, remember saving it into a file to keep possible changes.") 
    )
  )	

(tempo-define-template "custom-scheduler"
 '("-module(custom_scheduler)." n n
   "-export([init/1,choose/4,willcommit/1])." n n
   "-behaviour(mce_behav_scheduler)." n n
   "%% Scheduler state initializer" n
   "init(State) -> {ok,State}."n n
   "%% TODO: define it" n
   "willCommit(_) -> true." > n n
   "%% Called before every state transition in order to determine available transitions." n
   "%% Transitions :: [transition()]  List of all available transitions" n 
   "%% SchedState :: term() Scheduler private state" n
   "%% Monitor :: monitor() McErlang Monitor in use" n
   "%% Conf :: mce_opts() Configuration for current Model Checking run" n
   "choose(Transitions,SchedState,Monitor,Conf) ->" > n
   "FilteredTransitions =" > n
   "lists:filter" > n
   "(fun reqFilter/1," > n
   "	lists:map" > n
   "	(fun (T) -> mce_erl_opsem:commit(T,Monitor,Conf) end, Transitions))," > n
   "case length(FilteredTransitions) of" > n
   "N when N>0 ->" > n
   "%% Random selection of one of the filtered transitions" n
   "SelectedNumber = random:uniform(N)," > n
   "{ok,{lists:nth(SelectedNumber,FilteredTransitions),SchedState}};" > n
   "0 -> no_transitions" > n
   "end." > n n
   "%% Function to filter out undesired transitions" n
   "%% Now: transitions that require message sending" n
   "reqFilter({Actions,_}) -> not(lists:any(fun send/1, Actions))." > n n

   "%% Checks whether parameter action is a send statement."n
   "send(Action) ->" > n
   "case mce_erl_actions:is_send(Action) of" > n
   "true -> true;" > n
   "false -> false" > n
   "end." > n n
   )
)
















;; OPTIONS 

(tempo-define-template "mce-shortest-true" 
		       '(" , shortest= true " n ) 
)

(tempo-define-template "mce-shortest-false" 
		       '(" , shortest = false " n ) 
)


(tempo-define-template "mce-transitions" 
		       '(" , transitions = " (p "Function: ")  n ) 
)

(tempo-define-template "mce-commit" 
		       '(" , commit = " (p "Function: ")  n ) 
)

(tempo-define-template "mce-sim_external_world-true" 
		       '(" , sim_external_world = true "  n ) 
)

(tempo-define-template "mce-sim_external_world-false" 
		       '(" , sim_external_world = false "  n ) 
)

(tempo-define-template "mce-random-true" 
		       '(" , random = true "  n ) 
)

(tempo-define-template "mce-random-false" 
		       '(" , random = false "  n ) 
)

(tempo-define-template "mce-pathLimit" 
		       '(" , pathLimit = " (p "Maximum depth: ")   n ) 
)

(tempo-define-template "mce-terminate-true" 
		       '(" , terminate = true "  n ) 
)

(tempo-define-template "mce-terminate-false" 
		       '(" , terminate = false "  n ) 
)

(tempo-define-template "mce-is_simulation-true" 
		       '(" , is_simulation = true "  n ) 
)

(tempo-define-template "mce-is_simulation-false" 
		       '(" , is_simulation = false "  n ) 
)

(tempo-define-template "mce-small_pids-true" 
		       '(" , small_pids = true "  n ) 
)

(tempo-define-template "mce-small_pids-false" 
		       '(" , small_pids = false "  n ) 
)

(tempo-define-template "mce-notice_exits-true" 
		       '(" , notice_exits = true "  n ) 
)

(tempo-define-template "mce-notice_exits-false" 
		       '(" , notice_exits = false "  n ) 
)

(tempo-define-template "mce-fail_on_exit-true" 
		       '(" , fail_on_exit = true "  n ) 
)

(tempo-define-template "mce-fail_on_exit-false" 
		       '(" , fail_on_exit = false "  n ) 
)

(tempo-define-template "mce-sim_actions-true" 
		       '(" , sim_actions = true "  n ) 
)

(tempo-define-template "mce-sim_actions-false" 
		       '(" , sim_actions = false "  n ) 
)

(tempo-define-template "mce-output-true" 
		       '(" , output = true " n ) 
)

(tempo-define-template "mce-output-false" 
		       '(" , output = false "  n ) 
)

(tempo-define-template "mce-sim_keep_stack-true" 
		       '(" , sim_keep_stack = true "  n ) 
)

(tempo-define-template "mce-sim_keep_stack-false" 
		       '(" , sim_keep_stack = false "  n ) 
)

(tempo-define-template "mce-start_debugger-true" 
		       '(" , start_debugger = true "  n ) 
)

(tempo-define-template "mce-start_debugger-false" 
		       '(" , start_debugger = false "  n ) 
)

(tempo-define-template "mce-save_result-true" 
		       '(" , save_result = true " n ) 
)

(tempo-define-template "mce-save_result-false" 
		       '(" , save_result = false " n ) 
)

(tempo-define-template "mce-language-erlang" 
		       '(" , language = mce_erl_opsem "  n ) 
)


(tempo-define-template "mce-language-other" 
		       '(" , language = " (p "Language for Model Checker: ")  n ) 
)

(tempo-define-template "mce-distributed_semantics-true" 
		       '(" , distributed_semantics = true "  n ) 
)

(tempo-define-template "mce-distributed_semantics-false" 
		       '(" , distributed_semantics = false"  n ) 
)

(tempo-define-template "mce-chatter-normal" 
		       '(" , chatter = normal "  n ) 
)

(tempo-define-template "mce-chatter-verbose" 
		       '(" , chatter = verbose "  n ) 
)

(tempo-define-template "mce-time_limit" 
		       '(" , time_limit = " (p "Maximum verification time (seconds): " )  n ) 
)

(tempo-define-template "mce-is_infinitely_fast-false" 
		       '(" ,is_infinitely_fast  = false"  n ) 
)

(tempo-define-template "mce-is_infinitely_fast-true" 
		       '(" ,is_infinitely_fast  = true"  n ) 
)







;* Run McErlang

(tempo-define-template "mce-start-program" 
 '("mce:start(#mce_opts{"n" program={" (p "Module: ") "," (p "Function: ") ", ["(p "Parameters (separated by comma): ") "] }" n n  "} ).")
)


(defun mce-start-program ()
  (interactive)
  (tempo-template-mce-start-program)
  (message "Introduced options default structure. Remember you can edit the different options.")
)





;* Start McErlang Shell

(defun mymcerl ()
  (interactive)
  (shell)
  (rename-buffer "McErlangTemporal")
  (tempo-template-cd-ebin)
  (comint-send-input)
  (comint-run "mcerl")
  (kill-buffer "McErlangTemporal")
  (rename-buffer "McErlangShell")
  (sit-for 0.5)
  (load-mce_opts)
  (message "McErlang Shell: record mce_opts automatically loaded.")
  (comint-send-input)
)


(tempo-define-template "cd-ebin" 
 '("cd ebin")
)


;* Compile all Erlang files in directory

(defun compile ()
  (interactive)
  (shell)
  (rename-buffer "McErlangTemporal")
  (tempo-template-make-ebin)
  (comint-send-input)
  (tempo-template-compile-all)
  (comint-send-input)
  (sit-for 6)
  (kill-buffer "McErlangTemporal")
  (message "All Erlang files compiled.")
)


(tempo-define-template "compile-all" 
 '("mcerl_compiler -sources *.erl")
)

(tempo-define-template "make-ebin" 
 '("mkdir ebin")
)


;* Compiles and loads an Erlang file with def. opts.

(defun compile-file ()
  (interactive)
  (tempo-template-compile-file)
  (comint-send-input)
)


(tempo-define-template "compile-file" 
   '("mce_erl_compile:file(" (p "Relative path to source file: ") "/" (p "Module name: " module) ".erl)." n
   "erlang:load_module(" (s module) ")." n)
 )

;* Compiles and loads an Erlang file with param. opts.

(defun compile-file-options ()
  (interactive)
  (tempo-template-compile-file-options)
  (comint-send-input)
)


(tempo-define-template "compile-file-options" 
   '("mce_erl_compile:file(" (p "Relative path to source file: ") "/" (p "Module name: " module) ".erl, " (p "Options file: ") ")." n
   "erlang:load_module(" (s module) ")." n)
 )


;* Create a McErlang monitor skeleton

(defun create-safety-monitor () 
  "Creates a new buffer with a McErlang safety monitor structure"
  (interactive)
  (let ((new-buf (get-buffer-create "safety_monitor.erl")) 
	point)    
    (set-buffer new-buf)
    (erlang-mode)
    (switch-to-buffer new-buf)
    (tempo-template-safety-monitor)
    )
   (message "Buffer safety_monitor.erl created, remember saving it into a file to keep possible changes.") 
  )



(tempo-define-template "safety-monitor"
 '("-module(monitor)." n n
   "-export([init/1,stateChange/3,monitorType/0])." n n
   "-include(\"state.hrl\")." n n
   "-include(\"process.hrl\")." n n
   "-behaviour(mce_behav_monitor)." n n
   "%% monitorType :: atom() = safety | buechi" n
   "monitorType() -> safety." n n
   "%% Monitor state initializer" n
   "%% init( State :: state() ) -> {ok, initialState :: state() | Error}" n
   "init(State) -> "  n
   "{ok,State}." > n n
   "%% Evaluated after each system transition"n
   "%% State :: state() Current system state"n
   "%% MonState :: monState() Current monitor state (from previous transition)"n
   "%% Stack :: stack() System stack containing functions executed in state transition"n
   "stateChange(State,MonState,Stack) ->" > n
   "%% To be completed" n
   "{ok,MonState}." > n n
   n n
  )
)

(defun create-buchi-monitor () 
  "Creates a new buffer with a McErlang buchi monitor structure"
  (interactive)
  (let ((new-buf (get-buffer-create "buchi_monitor.erl")) 
	point)    
    (set-buffer new-buf)
    (erlang-mode)
    (switch-to-buffer new-buf)
    (tempo-template-buchi-monitor)
    )
   (message "Buffer buchi_monitor.erl created, remember saving it into a file to keep possible changes.") 
  )

(tempo-define-template "buchi-monitor"
 '("-module(monitor)." n n
   "-export([init/1,stateChange/3,monitorType/0,stateType/1])." n n
   "-include(\"state.hrl\")." n n
   "-include(\"process.hrl\")." n n
   "-behaviour(mce_behav_monitor)." n n
   "%% monitorType :: atom() = safety | buechi" n
   "monitorType() ->  buechi." n n
   "%% Monitor state initializer" n
   "%% init( State :: state() ) -> {ok, initialState :: state() }" n
   "init(State) -> "  n
   "{ok,State}." > n n
   "%% Evaluated after each system transition"n
   "%% State :: state() Current system state"n
   "%% MonState :: monState() Current monitor state (from previous transition)"n
   "%% Stack :: stack() System stack containing functions executed in state transition"n
   "stateChange(State,MonState,Stack) ->" > n
   "%% To be completed" n
   "{ok,MonState}." > n n
   "%% Must return either accepting or nonaccepting atom" n
   "stateType(_)->" > n
   "accepting." > n n
  )
)


(defun create-buchi-implication-monitor () 
  "Creates a new buffer with a McErlang buchi monitor structure for expression always (P implies Q)"
  (interactive)
  (let ((new-buf (get-buffer-create "buchi.erl")) 
	point)    
    (set-buffer new-buf)
    (erlang-mode)
    (switch-to-buffer new-buf)
    (tempo-template-buchi)
    )
   (message "Buffer buchi.erl created, remember saving it into a file to keep possible changes.") 
  )

(tempo-define-template "buchi"
 '("-module(buchi)." > n
"-language(erlang)." > n
"-behaviour(mce_behav_monitor)." > n

"-export([init/1,stateChange/3,monitorType/0,stateType/1])." > n n

"%% monitorType :: atom() = safety | buechi"  n
"monitorType() -> buechi." > n n

"%% Monitor state initializer"  n
"%% init( State :: state() ) -> {ok, initialState :: state() }"  n
"init(PState) ->" > n
"  {Priv,PredSpec}=PState," > n
"  {ok,{1,{Priv,mce_buechi_gen:parse_predspec(['P','Q'],PredSpec)}}}." > n n
"%% Evaluated after each system transition"  n
"%% Represents LTL formula \"not always (P implies Q)\"" n
"%% State :: state() Current system state" n
"%% MonState :: monState() Current monitor state (from previous transition)" n
"%% Stack :: stack() System stack containing functions executed in state transition" n
"stateChange(State,{MonState,{PrivateState,Predicates}},Stack) ->" > n
  "{Entry,_} = mce_behav_stackOps:pop(Stack)," > n
  "Actions = mce_buechi_gen:actions(Entry)," > n
  "case MonState of" > n
    "0 ->" > n
      "mce_buechi_gen:combine" > n
        "([mce_buechi_gen:ccond(State,Actions,PrivateState,{bool,true},0)],Predicates);" > n
    "1 ->" > n
      "mce_buechi_gen:combine" > n
        "([mce_buechi_gen:ccond(State,Actions,PrivateState,{bool,true},1)," > n
          "mce_buechi_gen:ccond(State,Actions,PrivateState,{cand,{{cnot,{pred,element(2,Predicates)}},{pred,element(1,Predicates)}}},0)],Predicates)" > n 
  "end." > n n
"%% Must return either accepting or nonaccepting atom" n
"stateType(0) -> accepting;" > n
"stateType(1) -> nonaccepting." > n
  )
)



 (defun create-deadlock-monitor () 
  "Creates a new buffer with a McErlang monitor structure"
  (interactive)
  (let ((new-buf (get-buffer-create "monitor_deadlock.erl")) 
	point)    
    (set-buffer new-buf)
    (switch-to-buffer new-buf)
    (erlang-mode)
    (tempo-template-deadlock-monitor)
    (message "Buffer monitor_deadlock.erl created, remember saving it into a file to keep possible changes.") 
    )
  )	

(tempo-define-template "deadlock-monitor"
 '("-module(monitor_deadlock)." n n
   "-export([init/1,stateChange/3,monitorType/0])." n n
   "-include(\"state.hrl\")." n n
   "-include(\"process.hrl\")." n n
   "-behaviour(mce_behav_monitor)." n n
   "%% monitorType :: atom() = safety | buchi" n
   "monitorType() -> safety." n n
   "%% Monitor state initializer" n
   "init(State) -> {ok,State}."n n
   "%% Evaluated after each system transition"n
   "%% State :: state() Current system state"n
   "%% MonState :: monState() Current monitor state"n
   "stateChange(State,MonState,_) ->"n
   "%% Check system state to identify a possible deadlock situation" > n 
   "case is_deadlocked(State) of"  > n
   "%% Deadlock identified. Execution will fail.%%" > n
   "true -> deadlock;" > n
   "%% Non deadlock situation. Monitor state returned unchanged" > n
   "false -> {ok, MonState}" > n
  "end." > n n
  "%% Checks system state in search for deadlock situations" > n
  "%% is_deadlocked :: bool()" > n
  "is_deadlocked(State) ->" n
    "State#state.ether =:= [] andalso" > n
    " case mce_erl:allProcesses(State) of" > n
    "[] -> false;" > n
    "Processes ->" > n
    " case mce_utils:find(fun (P) ->"  > n
    "P#process.status =/= blocked end,"> n
    "Processes) of" > n
    "%% False if there exist at least one process that is not blocked" > n
    " {ok, _} -> false;" > n
    " no -> true" > n
    "end" > n
    " end." > n n
      )
)


 





;**********************************************************END***********************