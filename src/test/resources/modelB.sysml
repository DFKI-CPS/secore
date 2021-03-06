bdd [package] acs [ACS]
-----------------------------------------------------------

block Person
  operations
    admitted(q: Door): Boolean
      post: q.org = self.sit and
        self.aut->includes(q.dest) and
        self.dap_dom->isEmpty()
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }
    dap_dom: Door[0..1] <- dap
  constraints
    inv: aut->forAll(b|self.aut.building->includes(b))
    inv: aut->includes(dap_dom.dest)
    inv: Person.allInstances()->
             forAll(p1, p2 |p1.dap_dom->notEmpty()
                        and p1.dap_dom = p2.dap_dom
                    implies p1 = p2)

block Building
  references
    gate: Building[*] <- building
      derive: org_dom.dest
    building: Building[*] <- gate
    org_dom: Door[*] <- org
  constraints
    inv: not gate->includes(self)

block Door
  values
    green: Boolean
      derive: dap->notEmpty()
    red: Boolean
  operations
    accept(p: Person)
      pre: not (green or red)
      pre: p.admitted(self)
      post: dap = p and p.dap_dom = self
    refuse(p: Person)
      pre: not (green or red)
      pre: not (p.admitted(self))
      post: red
    pass_thru()
      pre: green
      post: dap@pre.sit = dest
        and dap@pre.dap_dom->isEmpty
        and dap->isEmpty()
    off_grn()
      pre: green
      post: dap@pre.dap_dom->isEmpty()
        and dap->isEmpty()
    off_red()
      post: not red
  references
    dap:  Person[0..2] <- dap_dom
    org:  Building[1] <- org_dom
    dest: Building[1]
  ports
    incoming: Person
    passing: Person
  owned behaviors
    state machine DoorBehavior
      state Accepting
        after 30 seconds / off_grn() -> Waiting
        receive passing(p) / pass_thru() -> Waiting
      initial state Waiting
        receive incoming(p) / refuse(p) -> Refusing
        receive incoming(p) / accept(p) -> Accepting
      state Refusing
        after 30 seconds / off_red() -> Waiting
