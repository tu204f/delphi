unit UnitLineFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  Lb.Line;

type
  TLineTikets = class;
  TBlockLineTike = class;
  TTrades = class;

  TTypeBlock = (
    tbNormal, // Нейтральное состояние
    tbUp,     // Движение блока верх
    tbDown    // Движение блока вниз
  );


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

(******************************************************************************)
(* Каждое начало блока расматривать как отрытие позиции                       *)
(******************************************************************************)
  TStatusTrade = (stOpen, stClose);

  TTrade = class(TObject)
  private
    FOpenPrice: Double;
    FClosePrice: Double;
    FQuantity: Integer;
    FBuySell: Char;
    FProfit: Double;
    FStatus: TStatusTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpData(const APrice: Double);
    property OpenPrice: Double read FOpenPrice write FOpenPrice;
    property Quantity: Integer read FQuantity write FQuantity;
    property BuySell: Char read FBuySell write FBuySell;
    property ClosePrice: Double read FClosePrice;
    property Profit: Double read FProfit;
    property Status: TStatusTrade read FStatus write FStatus;
  end;
  TTradeList = TObjectList<TTrade>;

  TTrades = class(TObject)
  private
    FTrades: TTradeList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
    procedure SetUpData(const APrice: Double);
    property Items: TTradeList read FTrades;
  end;

(******************************************************************************)
(* Видимый блок                                                               *)
(******************************************************************************)

  TLineFrame = class(TFrame)
    Layout: TLayout;
    Rectangle: TRectangle;
    LayoutLine: TLayout;
    LayoutInd: TLayout;
    Timer: TTimer;
    TextStatusBlock: TText;
    GridPanelLayout: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    procedure TimerTimer(Sender: TObject);
  public const
    COUNT_PRICE_LINE = 200;
    LINE_STROKE_THICKNESS = 1;
  public type

    TPriceLine = class(TLayout)
    private
      FLine: TLine;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Progress(const AValue: Single);
    end;
    TPriceLineList = TObjectList<TPriceLine>;

  private
    FStepPrice: Double;
    FPriceLines: TPriceLineList;
    FLineTikets: TLineTikets;
  protected
    procedure SetPositionPriceLines;
    procedure SetInitializationPriceLines;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTiket(const APrice, AVol: Double);
    property StepPrice: Double write FStepPrice;
    property LineTikets: TLineTikets read FLineTikets;
  end;

implementation

{$R *.fmx}

// 'M50,0 L60,10 L20,50 L60,90 L50,100 L0,50 Z';

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

{ TTrade }

constructor TTrade.Create;
begin
  FOpenPrice := 0;
  FClosePrice := 0;
  FQuantity := 0;
  FBuySell := #0;
  FProfit := 0;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

procedure TTrade.SetUpData(const APrice: Double);
begin
  FClosePrice := APrice;
  case FBuySell of
    'B': FProfit := APrice - FOpenPrice;
    'S': FProfit := FOpenPrice - APrice;
  else
    raise Exception.Create('Error Message: Направление сделки не определена');
  end;
end;

{ TTrades }

constructor TTrades.Create;
begin
  FTrades := TTradeList.Create;
end;

destructor TTrades.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TTrades.OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);

  procedure _SetCloseTrade;
  begin
    for var xTrade in FTrades do
      xTrade.Status := TStatusTrade.stClose;
  end;

var
  xCount: Integer;
  xTrade: TTrade;
begin
  xCount := FTrades.Count;
  if xCount > 0 then
  begin
    xTrade := FTrades[xCount - 1];
    if xTrade.Profit < 0 then
      _SetCloseTrade;
  end;

  xTrade := TTrade.Create;
  xTrade.OpenPrice := APrice;
  xTrade.Quantity := AQuantity;
  xTrade.BuySell := ABuySell;
  FTrades.Add(xTrade);
end;

procedure TTrades.SetUpData(const APrice: Double);
begin
  for var xTrade in FTrades do
    if xTrade.Status = TStatusTrade.stOpen then
      xTrade.SetUpData(APrice);
end;

{ TLineFrame.TPriceLine }

constructor TLineFrame.TPriceLine.Create(AOwner: TComponent);
begin
  inherited;
  FLine := TLine.Create(Self);
  with FLine do
  begin
    Align := TAlignLayout.Left;
    Parent := Self;
    LineType := TLineType.Top;
    Stroke.Thickness := LINE_STROKE_THICKNESS;
  end;
end;

destructor TLineFrame.TPriceLine.Destroy;
begin
  FreeAndNil(FLine);
  inherited;
end;

procedure TLineFrame.TPriceLine.Progress(const AValue: Single);
begin
  FLine.Width := Self.Width * AValue;
end;


{ TLineFrame }

constructor TLineFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStepPrice := 1;
  FPriceLines := TPriceLineList.Create;
  FLineTikets := TLineTikets.Create;
end;

destructor TLineFrame.Destroy;
begin
  FreeAndNil(FLineTikets);
  FreeAndNil(FPriceLines);
  inherited;
end;

procedure TLineFrame.SetInitializationPriceLines;

  function _CreatePriceLine: TPriceLine;
  var
    xCount: Integer;
    xPriceLine: TPriceLine;
  begin
    xCount := FPriceLines.Count;
    xPriceLine := TPriceLine.Create(nil);
    with xPriceLine do
    begin
      Name := 'price_line_' + xCount.ToString;
      Parent := LayoutLine;
    end;
    Result := xPriceLine;
    FPriceLines.Add(xPriceLine);
  end;

var
  i, iCount: Integer;
begin
  FPriceLines.Clear;
  iCount := FLineTikets.Blocks.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
      _CreatePriceLine;
end;

procedure TLineFrame.AddTiket(const APrice, AVol: Double);
begin
  if FLineTikets.Count = 0 then
    FLineTikets.SetLimitPrice(APrice,FStepPrice);
  FLineTikets.UpDataPrice(APrice,AVol);
  // Изменилось количество видимых ценовых линий
  if (FPriceLines.Count <> FLineTikets.Blocks.Count) then
    SetInitializationPriceLines;
end;

procedure TLineFrame.SetPositionPriceLines;


  procedure _SetRectangle(ADeltaHeight: Single; ARectangle: TRectangle; ATrade: TTrade);
  var
    xIndTop, xIndBottom: Integer;
    xLeft, xTop, xWidth, xHeight: Single;
  begin
    xIndTop := FLineTikets.Blocks.IndexOf(ATrade.OpenPrice);
    xIndBottom := FLineTikets.Blocks.IndexOf(ATrade.ClosePrice);

    if xIndTop > xIndBottom then
    begin
      var xInd := xIndTop;
      xIndTop := xIndBottom;
      xIndBottom := xInd;
    end;

    xLeft := 0;
    xTop := xIndTop * ADeltaHeight;
    xWidth := Layout1.Width;
    xHeight := xIndBottom * ADeltaHeight - xTop;

    ARectangle.SetBounds(xLeft,xTop,xWidth,xHeight);

    if ATrade.Profit > 0 then
      ARectangle.Fill.Color := TAlphaColorRec.Green
    else
      ARectangle.Fill.Color := TAlphaColorRec.Red;
  end;

  procedure _SetTrades(ADeltaHeight: Single);
  var
    xTrade: TTrade;
    i, iCount, xInd: Integer;
  begin
    iCount := FLineTikets.Trades.Items.Count;
    if iCount > 0 then
    begin
      xInd := 0;
      for i := iCount - 1 downto 0 do
      begin
        xTrade := FLineTikets.Trades.Items[i];
        if xTrade.Status = TStatusTrade.stOpen then
        begin
          case xInd of
            3: _SetRectangle(ADeltaHeight, Rectangle1,xTrade);
            2: _SetRectangle(ADeltaHeight, Rectangle2,xTrade);
            1: _SetRectangle(ADeltaHeight, Rectangle3,xTrade);
            0: _SetRectangle(ADeltaHeight, Rectangle4,xTrade);
          end;
          Inc(xInd);
        end;
        if xInd >= 3 then
          Break;
      end;
    end;
  end;


var
  i, iCount: Integer;
  xPriceLine: TPriceLine;
  xLineTiket: TLineTiket;
  xMaxVol: Double;
  xX, xY, xWidth, xHeight, xDeltaHeight: Single;
  xProgressValue: Single;
begin
  iCount := FPriceLines.Count;
  if iCount > 0 then
  begin
    xMaxVol := FLineTikets.GetMaxVol;
    xDeltaHeight := LayoutLine.Height / iCount;

    // Ценовых уровений
    for i := 0 to iCount - 1 do
    begin
      xPriceLine := FPriceLines[i];
      xLineTiket := FLineTikets.Blocks.Items[i];

      xX := 0;
      xY := i * xDeltaHeight;
      xWidth := LayoutLine.Width;
      xHeight := xDeltaHeight;
      xPriceLine.SetBounds(xX, xY, xWidth, xHeight);

      xProgressValue := xLineTiket.Vol / xMaxVol;
      xPriceLine.Progress(xProgressValue);
    end;

    // Сделки
    _SetTrades(xDeltaHeight);

  end;
end;

procedure TLineFrame.TimerTimer(Sender: TObject);
begin
  Self.SetPositionPriceLines;
end;




end.
