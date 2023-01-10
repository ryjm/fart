::  fart: manual neighboring
::
|%
+$  records  ::  local state
  $:  outgoing=(jug ship @ta)
      incoming=(set ship)
    ::
      ::  receipts: for all outgoing, status
      ::
      ::    if ship not in receipts,  poke awaiting ack
      ::    if ship present as true,  poke acked positively
      ::    if ship present as false, poke acked negatively
      ::
      receipts=(map ship ?)
  ==
::
+$  gesture  ::  to/from others
  $%  [%rip ~]
      [%bye ~]
  ==
::
+$  mist  @t
+$  command  ::  from ourselves
  $%  [%dust =ship in=(set @ta)]  ::  empty set allowed
      [%wipe =ship in=(set @ta)]  ::  empty set implies un-targeting
  ==
::
+$  effect  ::  to ourselves
  $%  target-effect
      leeche-effect
  ==
::
+$  target-effect
  $%  [%dust =ship]  ::  hey to target
      [%wipe =ship]  ::  bye to target
  ==
::
+$  leeche-effect
  $%  [%hear =ship]  ::  hey from leeche
      [%wind =ship]  ::  bye from leeche
  ==
--
