unit UnitBlocksFrame;

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
  Lb.Blocks,
  Lb.Trades,
  UnitBlockFrame, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TBlockFrameList = TObjectList<TBlockFrame>;

  TBlocksFrame = class(TFrame)
    Layout: TLayout;
    LayoutTrades: TLayout;
    LayoutCells: TLayout;
    Rectangle: TRectangle;
    Timer: TTimer;
    Memo: TMemo;
    procedure TimerTimer(Sender: TObject);
  private
    FTrades: TTrades;
    FPriceMax: Double;
    FPriceMin: Double;
    FBlockFrames: TBlockFrameList;
    FBlocks: TBlocks;
    procedure EventBeginBlock(Sender: TObject);
    procedure EventEndBlock(Sender: TObject; APrice: Double; ATypeBlock: TTypeBlock);
    function GetPriceMax: Double;
    function GetPriceMin: Double;
  protected
    property PriceMax: Double read GetPriceMax;
    property PriceMin: Double read GetPriceMin;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTikit(const APrice, AVol: Double);
    property Trades: TTrades read FTrades;
  end;

implementation

{$R *.fmx}

{ TCellsFrame }

constructor TBlocksFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBlockFrames := TBlockFrameList.Create;

  FBlocks := TBlocks.Create;
  FBlocks.OnBeginBlock := EventBeginBlock;
  FBlocks.OnEndBlock   := EventEndBlock;

  FTrades := TTrades.Create;

end;

destructor TBlocksFrame.Destroy;
begin
  FreeAndNil(FTrades);
  FreeAndNil(FBlocks);
  FreeAndNil(FBlockFrames);
  inherited;
end;

procedure TBlocksFrame.EventBeginBlock(Sender: TObject);
var
  xBlock: TBlockFrame;
begin
  xBlock := TBlockFrame.Create(nil);
  xBlock.Parent := LayoutCells;
  FBlockFrames.Add(xBlock);
  Memo.Lines.Add('Event_Begin_Block: Добавить блок');
end;

procedure TBlocksFrame.EventEndBlock(Sender: TObject; APrice: Double; ATypeBlock: TTypeBlock);
var
  xS: String;
begin
  case ATypeBlock of
    tbNormal: xS := 'Normal';
    tbUp: xS := 'Up';
    tbDown: xS := 'Down';
  end;

  case ATypeBlock of
    tbUp: FTrades.OpenTrade(APrice,1,'B');
    tbDown: FTrades.OpenTrade(APrice,1,'S');
  end;


  Memo.Lines.Add('Event_End_Block: Обновление блока: Price = ' + APrice.ToString + ' '  + xS);
end;

procedure TBlocksFrame.SetTikit(const APrice, AVol: Double);
begin
  if FBlocks.Items.Count = 0 then
  begin
    FPriceMax := APrice + 500;
    FPriceMin := APrice - 500;
  end;
  // Обновляем условие сделки
  FTrades.SetUpData(APrice);
  FBlocks.UpData(APrice,Trunc(AVol));

  // Выводим информацию по сделки на экран

end;

function TBlocksFrame.GetPriceMax: Double;
var
  xMax: Double;
  i, iCount: Integer;
begin
  iCount := FBlocks.Items.Count;
  if iCount > 0 then
  begin
    xMax := FBlocks.Items[0].PriceMax;
    for i := 1 to iCount - 1 do
      if xMax < FBlocks.Items[i].PriceMax then
        xMax := FBlocks.Items[i].PriceMax;
    FPriceMax := xMax;
  end;
  Result := FPriceMax;
end;

function TBlocksFrame.GetPriceMin: Double;
var
  xMin: Double;
  i, iCount: Integer;
begin
  iCount := FBlocks.Items.Count;
  if iCount > 0 then
  begin
    xMin := FBlocks.Items[0].PriceMin;
    for i := 1 to iCount - 1 do
      if xMin > FBlocks.Items[i].PriceMin then
        xMin := FBlocks.Items[i].PriceMin;
    FPriceMin := xMin;
  end;
  Result := FPriceMin;
end;

procedure TBlocksFrame.TimerTimer(Sender: TObject);

  function _GetPriceY(const APrice: Double): Single;
  begin
    Result := LayoutCells.Height * (Self.PriceMax - APrice)/(Self.PriceMax - Self.PriceMin);
  end;

  procedure _SetTopAndBottom(const ABlockPriceMax, ABlockPriceMin: Double; var ATop, ABottom: Single);
  begin
    ATop := _GetPriceY(ABlockPriceMax);
    ABottom := _GetPriceY(ABlockPriceMin);
  end;

var
  xBlock: TBlock;
  xBlockFrame: TBlockFrame;
  i, iCount: Integer;

  xTop, xBottom: Single;
  xX, xY, xWidth, xHeight: Single;
begin
  iCount := FBlockFrames.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBlockFrame := FBlockFrames[i];
      xBlock := FBlocks.Items[i];
      xBlockFrame.SetBlock(xBlock);

      _SetTopAndBottom(xBlock.PriceMax, xBlock.PriceMin, xTop, xBottom);

      xX := i * 57;
      xY := xTop;
      xWidth := 55;
      xHeight := xBottom - xTop;

      xBlockFrame.SetBounds(xX,xY,xWidth,xHeight);
    end;
end;

end.
