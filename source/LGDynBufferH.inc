
  //TGCustomContainer special descendents

  { TGCustomArrayBuffer }

  generic TGCustomArrayBuffer<T> = class abstract(specialize TGCustomContainer<T>)
  protected
  type
    TCopyArrayHelper = class(specialize TGArrayHelpUtil<T>); // need access to protected functions

  var
    FItems: TArray;
    FCount: SizeInt;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
    procedure CheckEmpty; inline;
    procedure DoClear; override;
    function  IndexInRange(aIndex: SizeInt): Boolean; inline;
    function  IndexInInsertRange(aIndex: SizeInt): Boolean; inline;
    procedure CheckIndexRange(aIndex: SizeInt); inline;
    procedure CheckInsertIndexRange(aIndex: SizeInt); inline;
    property  ElemCount: SizeInt read FCount;
  public
    destructor Destroy; override;
  end;

  { TGCustomArrayContainer }

  generic TGCustomArrayContainer<T> = class abstract(specialize TGCustomArrayBuffer<T>)
  protected
  const
    // MAX_CAPACITY must be <= Succ(SizeInt.MaxValue div 2)
    MAX_CAPACITY: SizeInt = SizeInt(MAX_CONTAINER_SIZE div SizeOf(TItem));

  type
    TEnumerator = class(TContainerEnumerator)
    private
      FItems: TArray;
      FCurrIndex,
      FLast: SizeInt;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(c: TGCustomArrayContainer);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TReverseEnum = class(TContainerEnumerable)
    protected
      FItems: TArray;
      FCurrIndex,
      FCount: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(c: TGCustomArrayContainer);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    procedure Expand(aValue: SizeInt);
    procedure ItemAdding; inline;
    function  Append(constref aValue: T): SizeInt; inline;
    function  AppendArray(constref a: array of T): SizeInt;
    function  AppendEnum(e: IEnumerable): SizeInt;
    function  AppendContainer(aContainer: TCustomContainer): SizeInt;
    function  AppendEnumerable(e: IEnumerable): SizeInt;
    function  GetReverse: IEnumerable;
    function  DoGetEnumerator: TCustomEnumerator; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    procedure CopyItems(aBuffer: PItem); override;
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref a: array of T);
    constructor Create(e: IEnumerable);
    function  Reverse: IEnumerable; override;
    function  ToArray: TArray; override;
  end;

  { TGLiteDynBuffer: for internal use ONLY }
  generic TGLiteDynBuffer<T> = record
  const
    MAX_CAPACITY: SizeInt = SizeInt(MAX_CONTAINER_SIZE div SizeOf(T));

  type
    PGLiteDynBuffer = ^TGLiteDynBuffer;
    TArray = array of T;
    TItem  = T;
    PItem  = ^T;

    TEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FLast: SizeInt;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TMutableEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FLast: SizeInt;
      function  GetCurrent: PItem; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: PItem read GetCurrent;
    end;

    TMutable = record
    private
      FBuffer: PGLiteDynBuffer;
    public
      function GetEnumerator: TMutableEnumerator; inline;
    end;

    TReverseEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FCount: SizeInt;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;


    TReverse = record
    private
      FBuffer: PGLiteDynBuffer;
    public
      function GetEnumerator: TReverseEnumerator; inline;
    end;

  private
    function  GetCapacity: SizeInt; inline;
    procedure Expand(aValue: SizeInt); //inline;
    procedure CapacityExceedError(aValue: SizeInt); inline;
    procedure AccessEmptyError; inline;
    class operator Initialize(var b: TGLiteDynBuffer);
    class operator Finalize(var b: TGLiteDynBuffer);
    class operator Copy(constref aSrc: TGLiteDynBuffer; var aDst: TGLiteDynBuffer); inline;
  public
    FItems: TArray;
    FCount: SizeInt;
    procedure CheckEmpty; inline;
    procedure ItemAdding; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  GetEnumerator: TEnumerator; inline;
    function  GetMutableEnumerator: TMutableEnumerator; inline;
    function  GetReverseEnumerator: TReverseEnumerator; inline;
    function  Mutable: TMutable; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray; inline;
    function  PushLast(constref aValue: T): SizeInt; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  { TGCustomRingArrayBuffer }

  generic TGCustomRingArrayBuffer<T> = class abstract(specialize TGCustomArrayBuffer<T>)
  protected
  const
    // MAX_CAPACITY must be <= Succ(SizeInt.MaxValue div 2)
    MAX_CAPACITY: SizeInt = SizeInt(MAX_CONTAINER_SIZE div SizeOf(TItem));
  type

    TEnumerator = class(TContainerEnumerator)
    private
      FItems: TArray;
      FCurrIndex,
      FHead,
      FCount,
      FRest,
      FCapacity: SizeInt;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(c: TGCustomRingArrayBuffer);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TReverseEnum = class(TContainerEnumerable)
    protected
      FItems: TArray;
      FCurrIndex,
      FHead,
      FCount,
      FRest: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(c: TGCustomRingArrayBuffer);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FHead: SizeInt;
    procedure Expand(aValue: SizeInt);
    procedure Grow(aValue: SizeInt);
    procedure Shrink;
    procedure ItemAdding; inline;
    procedure Append(constref aValue: T);
    function  AppendArray(constref a: array of T): SizeInt;
    function  AppendContainer(aContainer: TCustomContainer): SizeInt;
    function  AppendEnum(e: IEnumerable): SizeInt;
    function  AppendEnumerable(e: IEnumerable): SizeInt;
    function  ExtractHead: T;
  { iterates items from head to tail }
    function  DoGetEnumerator: TCustomEnumerator; override;
    procedure DoClear; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    procedure CopyItems(aBuffer: PItem); override;
    property  Head: SizeInt read FHead;
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
    function  Reverse: IEnumerable; override;
  end;

  { TGLiteRingDynBuffer: for internal use ONLY }
  generic TGLiteRingDynBuffer<T> = record
  const
    MAX_CAPACITY: SizeInt = SizeInt(MAX_CONTAINER_SIZE div SizeOf(T));

  type
    PGLiteRingDynBuffer = ^TGLiteRingDynBuffer;
    TArray = array of T;
    TItem  = T;
    PItem  = ^T;

    TEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FHead,
      FCount,
      FRest,
      FCapacity: SizeInt;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TMutableEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FHead,
      FCount,
      FRest,
      FCapacity: SizeInt;
      function  GetCurrent: PItem; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: PItem read GetCurrent;
    end;

    TMutable = record
    private
      FBuffer: PGLiteRingDynBuffer;
    public
      function GetEnumerator: TMutableEnumerator; inline;
    end;

    TReverseEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FHead,
      FCount,
      FRest,
      FCapacity: SizeInt;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;


    TReverse = record
    private
      FBuffer: PGLiteRingDynBuffer;
    public
      function GetEnumerator: TReverseEnumerator; inline;
    end;

  private
    FCount,
    FHead: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Expand(aValue: SizeInt); //inline;
    procedure Grow(aValue: SizeInt);
    procedure Shrink;
    // do not check count !!!
    function  TailIndex: SizeInt; inline;
    procedure ItemAdding; inline;
    // do not check count !!!
    function  PopHead: T; inline;
    // do not check count !!!
    function  PopTail: T; inline;
    procedure CapacityExceedError(aValue: SizeInt); inline;
    procedure AccessEmptyError; inline;
    procedure CheckEmpty; inline;
    class operator Initialize(var b: TGLiteRingDynBuffer);
    class operator Finalize(var b: TGLiteRingDynBuffer);
    class operator Copy(constref aSrc: TGLiteRingDynBuffer; var aDst: TGLiteRingDynBuffer); inline;
  public
    FItems: TArray;
    // do not check bounds !!!
    function  InternalIndex(aIndex: SizeInt): SizeInt; inline;
    // do not check bounds !!!
    function  FastGetItem(aIndex: SizeInt): T; inline;
    // do not check bounds !!!
    procedure FastSetItem(aIndex: SizeInt; constref aValue: T); inline;
    procedure Clear; inline;
    procedure MakeEmpty;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  GetEnumerator: TEnumerator; inline;
    function  GetMutableEnumerator: TMutableEnumerator; inline;
    function  GetReverseEnumerator: TReverseEnumerator; inline;
    function  Mutable: TMutable; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray; inline;
    procedure PushLast(constref aValue: T);
    procedure PushFirst(constref aValue: T); inline;
    function  PopFirst: T; inline;
    function  TryPopFirst(out aValue: T): Boolean; inline;
    function  PopLast: T; inline;
    function  TryPopLast(out aValue: T): Boolean; inline;
    function  PeekFirst: T; inline;
    function  TryPeekFirst(out aValue: T): Boolean; inline;
    function  PeekLast: T; inline;
    function  TryPeekLast(out aValue: T): Boolean; inline;
    property  Head: SizeInt read FHead;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;




