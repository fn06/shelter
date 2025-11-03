@0xf906d3ebde714337;

enum Resource {
    file  @0;
    git   @1;
    ptr   @2;
    unit  @3;
    error @4;
}

interface Resolver {
  resolve @0 (vurl: Text, resource: Resource) -> (vurl: Text);
}