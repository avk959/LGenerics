program AllTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, consoletestrunner,
  LGUtilsTest,
  LGArrayHelpUtilTest,
  LGBaseArrayHelperTest,
  LGComparableArrayHelperTest,
  LGDelegatedArrayHelperTest,
  LGRegularArrayHelperTest,
  LGNestedArrayHelperTest,
  LGNumArrayHelperTest,
  EnumerableTest,
  LGStackTest,
  LGQueueTest,
  LGVectorTest,
  LGDequeTest,
  LGPriorityQueueTest,
  LGListTest,
  LGHashTest,
  LGHashSetTest,
  LGTreeSetTest,
  LGHashMultiSetTest,
  LGTreeMultisetTest,
  LGHashMapTest,
  LGTreeMapTest,
  LGMultimapTest,
  LGBiMapTest,
  LGTable2DTest,
  LGRootTreeTest,
  LGSparseGraphTest,
  LGSimpleGraphTest,
  LGSimpleDiGraphTest,
  LGInt128Test;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'AllTests';
  Application.Run;
  Application.Free;
end.
