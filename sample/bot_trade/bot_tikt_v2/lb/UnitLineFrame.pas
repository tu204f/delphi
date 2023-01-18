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
  Lb.Trades,
  Lb.Line;

type
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
