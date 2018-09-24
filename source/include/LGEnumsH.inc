
  //IGEnumerable special descendents

  { TGAutoEnumerable }

  generic TGAutoEnumerable<T> = class abstract(specialize TGCustomEnumerator<T>, specialize IGEnumerable<T>)
  protected
  type
    TEnumerable = class(specialize TGEnumerable<T>, specialize IGEnumerable<T>, IObjInstance)
    protected
      FOwner: TGAutoEnumerable;
    public
      constructor Create(e: TGAutoEnumerable);
      function GetEnumerator: TCustomEnumerator; override;
    end;

  public
  type
    IEnumerable = TEnumerable.IEnumerable;
    TCustomEnumerator = TEnumerable.TCustomEnumerator;

  protected
  var
    FEnumerable: TEnumerable;
    property Enumerable: TEnumerable read FEnumerable implements IEnumerable;
  public
    constructor Create;
    destructor  Destroy; override;
  end;

  { TGEnumCursor }

  generic TGEnumCursor<T> = class(specialize TGAutoEnumerable<T>)
  protected
    FEnum: TCustomEnumerator;
    function  GetCurrent: T; override;
  public
    constructor Create(e: TCustomEnumerator);
    destructor  Destroy; override;
    function  MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  { TGEnumRegularFilter }

  generic TGEnumRegularFilter<T> = class(specialize TGEnumCursor<T>)
  public
  type
    TTest = TEnumerable.TTest;

  private
    FTest: TTest;
  public
    constructor Create(e: TCustomEnumerator; aTest: TTest);
    function MoveNext: Boolean; override;
  end;

  { TGEnumDelegatedFilter }

  generic TGEnumDelegatedFilter<T> = class(specialize TGEnumCursor<T>)
  public
  type
    TOnTest = TEnumerable.TOnTest;

  private
    FTest: TOnTest;
  public
    constructor Create(e: TCustomEnumerator; aTest: TOnTest);
    function MoveNext: Boolean; override;
  end;

  { TGEnumNestedFilter }

  generic TGEnumNestedFilter<T> = class(specialize TGEnumCursor<T>)
  public
  type
    TNestTest = TEnumerable.TNestTest;

  private
    FTest: TNestTest;
  public
    constructor Create(e: TCustomEnumerator; aTest: TNestTest);
    function MoveNext: Boolean; override;
  end;

  { TGEnumRegularMap }

  generic TGEnumRegularMap<T> = class(specialize TGEnumCursor<T>)
  public
  type
    TMapFunc = TEnumerable.TMapFunc;

  private
    FMap: TMapFunc;
  public
    constructor Create(e: TCustomEnumerator; aMap: TMapFunc);
    function GetCurrent: T; override;
  end;

  { TGEnumDelegatedMap }

  generic TGEnumDelegatedMap<T> = class(specialize TGEnumCursor<T>)
  public
  type
    TOnMap = TEnumerable.TOnMap;

  private
    FMap: TOnMap;
  public
    constructor Create(e: TCustomEnumerator; aMap: TOnMap);
    function GetCurrent: T; override;
  end;

  { TGEnumNestedMap }

  generic TGEnumNestedMap<T> = class(specialize TGEnumCursor<T>)
  public
  type
    TNestMap = TEnumerable.TNestMap;

  private
    FMap: TNestMap;
  public
    constructor Create(e: TCustomEnumerator; aMap: TNestMap);
    function GetCurrent: T; override;
  end;

  { TGSkipEnumerable }

  generic TGSkipEnumerable<T> = class(specialize TGEnumCursor<T>)
  private
    FSkipCount,
    FSkipped: SizeInt;
    procedure Skip; inline;
  public
    constructor Create(e: TCustomEnumerator; aCount: SizeInt);
    procedure Reset; override;
  end;

  { TGLimitEnumerable }

  generic TGLimitEnumerable<T> = class(specialize TGEnumCursor<T>)
  private
    FLimit,
    FCurrent: SizeInt;
  public
    constructor Create(e: TCustomEnumerator; aCount: SizeInt);
    function  MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  { TGArrayCursor }

  generic TGArrayCursor<T> = class(specialize TGAutoEnumerable<T>)
  public
  type
    TArray = TEnumerable.TArray;

  protected
  type
    TArrayEnumerable = class(TEnumerable)
      function ToArray: TArray; override;
    end;

  var
    FArray: TArray;
    FCurrIndex: SizeInt;
    function  GetCurrent: T; override;
  public
    constructor Create(constref a: TArray);
    function  MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  { TGArrayReverse }

  generic TGArrayReverse<T> = class(specialize TGAutoEnumerable<T>)
  public
  type
    TArray = TEnumerable.TArray;

  protected
  type
    TArrayEnumerable = class(TEnumerable)
      function ToArray: TArray; override;
    end;

  var
    FArray: TArray;
    FCurrIndex: SizeInt;
    function  GetCurrent: T; override;
  public
    constructor Create(constref a: TArray);
    function  MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  { TGArrayEnumerator }

  generic TGArrayEnumerator<T> = class(specialize TGCustomEnumerator<T>)
  public
  type
    TArray = specialize TGArray<T>;

  protected
    FArray: TArray;
    FPosition,
    FLast: SizeInt;
    function  GetCurrent: T; override;
  public
    constructor Create(constref a: TArray);
    function  MoveNext: Boolean; override;
    procedure Reset; override;
  end;
