::  fart: a fart app
::
::  shameless %pals rebrand, with farts
::
/-  *fart
/+  rudder, dbug, verb, default-agent
::
/~  pages  (page:rudder records command)  /app/fart/webui
::
|%
+$  state-0  [%0 records]
+$  eyre-id  @ta
+$  card  (wind note gift)
+$  gift  gift:agent:gall
+$  note  note:agent:gall
  :: $%  [%agent [ship %fart] task:agent:gall]
  ::     [%arvo %e %connect [~ %fart ~] term]
  :: ==
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =^  cards  this
    (on-poke %fart-command !>(`command`[%dust ~littel-wolfur ~]))
  =-  [[- cards] this]
  [%pass /eyre/connect %arvo %e %connect [~ /[dap.bowl]] dap.bowl]
::
++  on-save  !>(state)
::
++  on-load
  |=  ole=vase
  ^-  (quip card _this)
  =/  old=state-0  !<(state-0 ole)
  [~ this(state old)]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+  mark  (on-poke:def mark vase)
    ::  %fart-command: local app control
    ::
      %fart-command
    ?>  =(our src):bowl
    =+  !<(cmd=command vase)
    ?:  (~(has in in.cmd) ~.)
      ~|  [%illegal-empty-list-name in=-.cmd]
      !!
    ?:  =(our.bowl ship.cmd)
      [~ this]
    ::
    =/  known=?  (~(has by outgoing) ship.cmd)
    =;  [yow=? =_outgoing]
      ^-  (quip card _this)
      :_  this(outgoing.state outgoing)
      ?.  yow  ~
      :~  =/  =gesture  ?-(-.cmd %dust [%rip ~], %wipe [%bye ~])
          =/  =cage     [%fart-gesture !>(gesture)]
          [%pass /[-.gesture] %agent [ship.cmd dap.bowl] %poke cage]
        ::
          =/  =effect   ?-(-.cmd %dust [- ship]:cmd, %wipe [- ship]:cmd)
          =/  =cage     [%fart-effect !>(effect)]
          [%give %fact [/targets]~ cage]
      ==
    ::
    ?-  -.cmd
        %dust
      :-  !known
      %+  ~(put by outgoing)  ship.cmd
      %-  ~(uni in in.cmd)
      (~(gut by outgoing) ship.cmd ~)
    ::
        %wipe
      ?:  =(~ in.cmd)
        ::  remove target entirely
        ::
        [known (~(del by outgoing) ship.cmd)]
      ::  remove from specified lists
      ::
      :-  |
      =.  outgoing
        =/  liz=(list @ta)  ~(tap in in.cmd)
        |-  ^+  outgoing
        ?~  liz  outgoing
        $(liz t.liz, outgoing (~(del ju outgoing) ship.cmd i.liz))
      ::NOTE  we could account for this above, but +del:ju is just easier there
      =?  outgoing  !(~(has by outgoing) ship.cmd)
        (~(put by outgoing) ship.cmd ~)
      outgoing
    ==
  ::
    ::  %fart-gesture: foreign %fart signals
    ::
      %fart-gesture
    ?<  =(our src):bowl
    =*  ship  src.bowl
    =+  !<(=gesture vase)
    =/  [yow=? =_incoming]
      =*  has  (~(has in incoming) ship)
      ?-  -.gesture
        %rip  :-  !has  (~(put in incoming) ship)
        %bye  :-   has  (~(del in incoming) ship)
      ==
    :_  this(incoming.state incoming)
    ^-  (list card)
    ?.  yow  ~
    :*  =/  =effect  ?-(-.gesture %rip [%hear ship], %bye [%wind ship])
        =/  =cage    [%fart-effect !>(effect)]
        [%give %fact [/leeches]~ cage]
      ::
        ?.  .^(? %gu /(scot %p our.bowl)/hark/(scot %da now.bowl))  ~
        =/  body
          =-  [ship+ship - ~]
          ?-  -.gesture
            %rip  ': *FRAAAAP*'
            %bye  ' could not handle your awful stench'
          ==
        =/  id      (end 7 (shas %fart-notification eny.bowl))
        =/  rope    [~ ~ q.byk.bowl /(scot %p ship)/[-.gesture]]
        =/  action  [%add-yarn & & id rope now.bowl body /fart ~]
        =/  =cage   [%hark-action !>(action)]
        [%pass /hark %agent [our.bowl %hark] %poke cage]~
    ==
  ::
    ::  %handle-http-request: incoming from eyre
    ::
      %handle-http-request
    =;  out=(quip card _+.state)
      [-.out this(+.state +.out)]
    %.  [bowl !<(order:rudder vase) +.state]
    %-  (steer:rudder _+.state command)
    :^    pages
        (point:rudder /[dap.bowl] & ~(key by pages))
      (fours:rudder +.state)
    |=  cmd=command
    ^-  $@  brief:rudder
        [brief:rudder (list card) _+.state]
    =^  caz  this
      (on-poke %fart-command !>(cmd))
    ['got em' caz +.state]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  =(our.bowl src.bowl)
  ?+  path  (on-watch:def path)
    [%http-response *]  [~ this]
  ::
      [%targets ~]
    :_  this
    %+  turn  ~(tap in ~(key by outgoing))
    |=(=@p [%give %fact ~ %fart-effect !>(`effect`[%dust p])])
  ::
      [%leeches ~]
    :_  this
    %+  turn  ~(tap in incoming)
    |=(=@p [%give %fact ~ %fart-effect !>(`effect`[%hear p])])
  ::
    ::TODO  consider adding a subscription endpoint that includes tags?
    ::      shouldn't become too legible to applications though...
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  wire  ~&([dap.bowl %strange-wire wire] [~ this])
      [%hark ~]
    ?.  ?=(%poke-ack -.sign)  (on-agent:def wire sign)
    ?~  p.sign  [~ this]
    ((slog 'fart: failed to notify' u.p.sign) [~ this])
  ::
      [%bye ~]  [~ this]  ::TODO  also retry if nack?
      [%rip ~]
    ::  for %fart-gesture pokes, record the result
    ::TODO  should we slowly retry for nacks?
    ::
    =-  [~ this(receipts -)]
    ?+  -.sign  ~|([%unexpected-agent-sign wire -.sign] !!)
      %poke-ack  (~(put by receipts) src.bowl ?=(~ p.sign))
    ==
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?>  =(our src):bowl
  |^  ?+  path  [~ ~]
        [%y ~]                 (arc %leeches %targets %mutuals ~)
        [%y %leeches ~]        (arc ~)
        [%y %targets ~]        (arc (las targets))
        [%y %mutuals ~]        (arc (las mutuals))
        [%x %leeches ~]        (alp leeches)
        [%x %leeches @ ~]      (ask (bind (slaw %p i.t.t.path) (sin leeches)))
        [%x %targets ~]        (alp targets)
        [%x %targets ~ ~]      [~ ~]
        [%x %targets @ta ~]    (alp (lap targets i.t.t.path))
        [%x %targets @ta @ ~]  (ask (bind (wat t.t.path) (hal targets)))
        [%x %mutuals ~]        (alp mutuals)
        [%x %mutuals ~ ~]      [~ ~]
        [%x %mutuals @ta ~]    (alp (lap mutuals i.t.t.path))
        [%x %mutuals @ta @ ~]  (ask (bind (wat t.t.path) (hal mutuals)))
      ::
          [%x %json ~]  ::NOTE  dumb hack, subject to change
        =;  =json  ``json+!>(json)
        =,  enjs:format
        %-  pairs
        :~  :-  'outgoing'
            %-  pairs
            %+  turn  ~(tap by outgoing)
            |=  [=^ship lists=(set @ta)]
            :-  (rsh 3 (scot %p ship))
            %-  pairs
            :~  'lists'^a+(turn ~(tap in lists) (lead %s))
                'ack'^(fall (bind (~(get by receipts) ship) (lead %b)) ~)
            ==
          ::
            :-  'incoming'
            %-  pairs
            %+  turn  ~(tap in incoming)
            |=(=^^ship [(rsh 3 (scot %p ship)) b+&])
        ==
      ==
  ::  scry results
  ++  arc  |=  l=(list @ta)  ``noun+!>(`arch`~^(malt (turn l (late ~))))
  ++  alp  |=  s=(set @p)    ``noun+!>(s)
  ++  alf  |=  f=?           ``noun+!>(f)
  ++  ask  |=  u=(unit ?)  ?^(u (alf u.u) [~ ~])
  ::  data wrestling
  ++  wat  |=([l=@ta p=@ta ~] ?~(p=(slaw %p p) ~ (some [l u.p])))
  ++  nab  ~(got by outgoing)
  ++  las  |=(s=(set @p) (zing (turn (sit s) |=(=@p (sit (nab p))))))
  ++  lap  |=([s=(set @p) l=@ta] (ski s |=(=@p ((sin (nab p)) l))))
  ++  hal  |=(s=(set @p) |=([l=@ta =@p] ((sin ?~(l s (lap s l))) p)))
  ::  set shorthands
  ++  sin  |*(s=(set) ~(has in s))
  ++  sit  |*(s=(set) ~(tap in s))
  ++  ski  |*([s=(set) f=$-(* ?)] (sy (skim (sit s) f)))
  ::  fart
  ++  leeches  incoming
  ++  targets  ~(key by outgoing)
  ++  mutuals  (~(int in targets) leeches)
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  sign-arvo  (on-arvo:def wire sign-arvo)
      [%eyre %bound *]
    ~?  !accepted.sign-arvo
      [dap.bowl 'eyre bind rejected!' binding.sign-arvo]
    [~ this]
  ==
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--

