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
  FMX.Objects;

(*
    object Path: TPath
      Data.Path = {
        0700000000000000000048420000000001000000000070420000204101000000
        0000A0410000484201000000000070420000B44201000000000048420000C842
        010000000000000000004842030000000000484200000000}
      Position.X = 116.000000000000000000
      Position.Y = 236.000000000000000000
      Size.Width = 25.000000000000000000
      Size.Height = 25.000000000000000000
      Size.PlatformDefault = False
      WrapMode = Fit
    end
*)

type
  TLineTikets = class;
  TBlockLineTike = class;

  TLineTiket = record
    Price: Double;
    Vol: Double;
  public
    constructor Create(const APrice, AVol: Double);
  end;
  TLineTiketList = TList<TLineTiket>;

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
(* Весь массив - динамики. Предельная значение массива 5000                   *)
(******************************************************************************)

  TLineTikets = class(TCustomLineTikets)
  private
    FBlocks: TBlockLineTike;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Обновляет объем по цены</summary>
    function UpDatePrice(const APrice: Double; AVol: Double): Integer;
    ///<summary>Формируется массив</summary>
    procedure SetLimitMaxAndMin(const AMaxPrice, AMinPrice, AStepPrice: Double);
    procedure SetLimitPrice(const APrice, AStepPrice: Double; ACount: Integer = 5000);
    function GetMaxVol: Double;
    property Blocks: TBlockLineTike read FBlocks;
  end;

(******************************************************************************)
(* Влок видимости данных - для анализа *)
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
    procedure SetIndex(AIndex: Integer);
  public
    constructor Create(ALineTikets: TLineTikets);
    destructor Destroy; override;
    property Items[Index: Integer]: TLineTiket read GetItems;
    property Count: Integer read GetCount;
    {Полу предел, так как все придел устанавливается Limit * 2}
    ///<summary>Максимальный придел</summary>
    property MaxLimit: Integer read FMaxLimit write FMaxLimit;
    ///<summary>Придел смешение</summary>
    property MoveLimit: Integer read FMoveLimit write FMoveLimit;
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

{ TLineTikets }

constructor TLineTikets.Create;
begin
  inherited;
  FBlocks := TBlockLineTike.Create(Self);
end;

destructor TLineTikets.Destroy;
begin
  FreeAndNil(FBlocks);
  inherited;
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

function TLineTikets.UpDatePrice(const APrice: Double; AVol: Double): Integer;
var
  xTiket: TLineTiket;
  xIndex: Integer;
begin
  xIndex := IndexOfLine(APrice);
  if xIndex >= 0 then
  begin
    xTiket := FTikets[xIndex];
    xTiket.Vol := xTiket.Vol + AVol;
    FTikets[xIndex] := xTiket;
    Result := xIndex;
    FBlocks.SetIndex(xIndex);
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

procedure TBlockLineTike.SetIndex(AIndex: Integer);
begin
  if (FTopIndex = 0) and (FBottomIndex = 0) then
  begin
    FTopIndex := AIndex - FMaxLimit;
    FBottomIndex := AIndex + FMaxLimit;
  end
  else if (FTopIndex + FMoveLimit) > AIndex then
  begin
    FTopIndex := AIndex - FMaxLimit;
    FBottomIndex := AIndex + FMaxLimit;
  end
  else if (FBottomIndex - FMoveLimit) < AIndex then
  begin
    FTopIndex := AIndex - FMaxLimit;
    FBottomIndex := AIndex + FMaxLimit;
  end;
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
  FLineTikets.UpDatePrice(APrice,AVol);
  // Изменилось количество видимых ценовых линий
  if (FPriceLines.Count <> FLineTikets.Blocks.Count) then
    SetInitializationPriceLines;
end;

procedure TLineFrame.SetPositionPriceLines;
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
  end;
end;



procedure TLineFrame.TimerTimer(Sender: TObject);
begin
  Self.SetPositionPriceLines;
end;





end.
