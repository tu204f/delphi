unit Lb.Line;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Trades;

type
  TLineTikets = class;
  TBlockLineTike = class;

  TTypeBlock = (
    tbNormal, // Нейтральное состояние
    tbUp,     // Движение блока верх
    tbDown    // Движение блока вниз
  );

  ///<summary>Ценовой уровень</summary>
  TLineTiket = record
    Price: Double;
    Vol: Double;
  public
    constructor Create(const APrice, AVol: Double);
  end;
  TLineTiketList = TList<TLineTiket>;

  ///<summary><fpjdsq </summary>
  TCustomLineTikets = class(TObject)
  private
    FTikets: TLineTiketList;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function IndexOfLine(const APrice: Double): Integer;
    property Tikets: TLineTiketList read FTikets;
    property Count: Integer read GetCount;
  end;

(******************************************************************************)
(* Структура данных которые нужно модефирировать                              *)
(******************************************************************************)

  TParam = class(TObject)
  private

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

(******************************************************************************)
(* Весь массив - динамики. Предельная значение массива 5000                   *)
(******************************************************************************)

  TOnChangeBlock = procedure(Sender: TObject; APrice: Double; ATypeBlock: TTypeBlock) of object;

  TLineTikets = class(TCustomLineTikets)
  private
    FBlocks: TBlockLineTike;
    FTrades: TTrades;
    FOnChangeBlock: TOnChangeBlock;
  protected
    procedure DoChangeBlock(APrice: Double; ATypeBlock: TTypeBlock);
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Обновляет объем по цены</summary>
    function UpDataPrice(const APrice: Double; AVol: Double): Integer;
    ///<summary>Формируется массив</summary>
    procedure SetLimitMaxAndMin(const AMaxPrice, AMinPrice, AStepPrice: Double);
    procedure SetLimitPrice(const APrice, AStepPrice: Double; ACount: Integer = 5000);
    function GetMaxVol: Double;
    property Blocks: TBlockLineTike read FBlocks;
    property Trades: TTrades read FTrades;
    property OnChangeBlock: TOnChangeBlock write FOnChangeBlock;
  end;

(******************************************************************************)
(* Влок видимости данных - для анализа                                        *)
(******************************************************************************)

  TBlockLineTike = class(TObject)
  public const
    MAX_LIMIT = 100;
  private
    FLineTikets: TLineTikets;
    FTopIndex: Integer;
    FBottomIndex: Integer;
    FMaxLimit: Integer;
    FMoveLimit: Integer;
    function GetCount: Integer;
    function GetItems(Index: Integer): TLineTiket;
  protected
    function GetIndex(AIndex: Integer; APrice: Double): Boolean;
  public
    constructor Create(ALineTikets: TLineTikets);
    destructor Destroy; override;
    function IndexOf(const APrice: Double): Integer;
    property Items[Index: Integer]: TLineTiket read GetItems;
    property Count: Integer read GetCount;
    {Полу предел, так как все придел устанавливается Limit * 2}
    ///<summary>Максимальный придел</summary>
    property MaxLimit: Integer read FMaxLimit write FMaxLimit;
    ///<summary>Придел смешение</summary>
    property MoveLimit: Integer read FMoveLimit write FMoveLimit;
  end;

implementation

{ TLineTiket }

constructor TLineTiket.Create(const APrice, AVol: Double);
begin
  Price := APrice;
  Vol := AVol;
end;

{ TCustomLineTikets }

constructor TCustomLineTikets.Create;
begin
  FTikets := TLineTiketList.Create;
end;

destructor TCustomLineTikets.Destroy;
begin
  FreeAndNil(FTikets);
  inherited;
end;

function TCustomLineTikets.GetCount: Integer;
begin
  Result := FTikets.Count;
end;

procedure TCustomLineTikets.Clear;
begin
  FTikets.Clear;
end;

function TCustomLineTikets.IndexOfLine(const APrice: Double): Integer;
var
  xTiket: TLineTiket;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := FTikets.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTiket := FTikets[i];
      if xTiket.Price = APrice then
      begin
        Result := i;
        Break;
      end;
    end;
end;

{ TParam }

constructor TParam.Create;
begin

end;

destructor TParam.Destroy;
begin

  inherited;
end;

{ TLineTikets }

constructor TLineTikets.Create;
begin
  inherited;
  FBlocks := TBlockLineTike.Create(Self);
  FTrades := TTrades.Create;
end;

destructor TLineTikets.Destroy;
begin
  FreeAndNil(FTrades);
  FreeAndNil(FBlocks);
  inherited;
end;

procedure TLineTikets.DoChangeBlock(APrice: Double; ATypeBlock: TTypeBlock);
begin
  case ATypeBlock of
    tbUp: FTrades.OpenTrade(APrice,1,'B');
    tbDown: FTrades.OpenTrade(APrice,1,'S');
  end;
  if Assigned(FOnChangeBlock) then
    FOnChangeBlock(Self,APrice,ATypeBlock);
end;

function TLineTikets.GetMaxVol: Double;
var
  xMaxVol: Double;
  xTiket: TLineTiket;
  i, iCount: Integer;
begin
  xMaxVol := 0;
  iCount := FTikets.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTiket := FTikets[i];
      if xMaxVol < xTiket.Vol then
        xMaxVol := xTiket.Vol;
    end;
  Result := xMaxVol;
end;

procedure TLineTikets.SetLimitMaxAndMin(const AMaxPrice, AMinPrice, AStepPrice: Double);
var
  xPrice: Double;
  xTiket: TLineTiket;
begin
  FTikets.Clear;
  xPrice := AMaxPrice;
  while xPrice >= AMinPrice do
  begin
    xTiket := TLineTiket.Create(xPrice,0);
    FTikets.Add(xTiket);
    xPrice := xPrice - AStepPrice;
  end;
end;

procedure TLineTikets.SetLimitPrice(const APrice, AStepPrice: Double; ACount: Integer);
var
  xCountInTwo: Integer;
  xMaxPrice, xMinPrice: Double;
begin
  xCountInTwo := Trunc(ACount/2);
  xMaxPrice := APrice + xCountInTwo * AStepPrice;
  xMinPrice   := APrice - (ACount - xCountInTwo) * AStepPrice;
  SetLimitMaxAndMin(xMaxPrice,xMinPrice,AStepPrice);
end;

function TLineTikets.UpDataPrice(const APrice: Double; AVol: Double): Integer;

  procedure _ClearLineTiket;
  var
    xTiket: TLineTiket;
  begin
    for var i := 0 to FTikets.Count - 1 do
    begin
      xTiket := FTikets[i];
      xTiket.Vol := 0;
      FTikets[i] := xTiket;
    end;
  end;

var
  xTiket: TLineTiket;
  xIndex: Integer;
begin
  FTrades.SetUpData(APrice);
  xIndex := IndexOfLine(APrice);
  if xIndex >= 0 then
  begin
    if FBlocks.GetIndex(xIndex,APrice) then
      _ClearLineTiket;


    xTiket := FTikets[xIndex];
    xTiket.Vol := xTiket.Vol + AVol;
    FTikets[xIndex] := xTiket;
    Result := xIndex;

  end
  else
    raise Exception.Create('Error Message: Вышли за пределы массива');
end;


{ TBlockLineTike }

constructor TBlockLineTike.Create(ALineTikets: TLineTikets);
begin
  FLineTikets := ALineTikets;
  FTopIndex := 0;
  FBottomIndex := 0;
  FMaxLimit := MAX_LIMIT;
  FMoveLimit := 20;
end;

destructor TBlockLineTike.Destroy;
begin
  FLineTikets := nil;
  inherited;
end;

function TBlockLineTike.GetCount: Integer;
begin
  Result := (FBottomIndex - FTopIndex + 1);
end;

function TBlockLineTike.GetItems(Index: Integer): TLineTiket;
var
  xIndex: Integer;
begin
  xIndex := FTopIndex + Index;
  if (xIndex >= 0) and (xIndex < FLineTikets.Tikets.Count) then
    Result := FLineTikets.Tikets[xIndex]
  else
    raise Exception.Create('Error Message: Превышение придела массива');
end;

function TBlockLineTike.IndexOf(const APrice: Double): Integer;
var
  i, iCount: Integer;
begin
  Result := -1;
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
      if Self.Items[i].Price = APrice then
      begin
        Result := i;
        Break;
      end;
end;

function TBlockLineTike.GetIndex(AIndex: Integer; APrice: Double): Boolean;
begin
  Result := False;
  if (FTopIndex = 0) and (FBottomIndex = 0) then
  begin
    FTopIndex := AIndex - FMaxLimit;
    FBottomIndex := AIndex + FMaxLimit;
  end
  else if (FTopIndex + FMoveLimit) > AIndex then
  begin
    // Ростем
    Result := True;
    FTopIndex := AIndex - FMaxLimit;
    FBottomIndex := AIndex + FMaxLimit;
    FLineTikets.DoChangeBlock(APrice, TTypeBlock.tbUp);
  end
  else if (FBottomIndex - FMoveLimit) < AIndex then
  begin
    // Падаем
    Result := True;
    FTopIndex := AIndex - FMaxLimit;
    FBottomIndex := AIndex + FMaxLimit;
    FLineTikets.DoChangeBlock(APrice, TTypeBlock.tbDown);
  end;
end;



end.
