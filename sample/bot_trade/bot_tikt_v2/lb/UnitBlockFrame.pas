unit UnitBlockFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  Lb.Blocks;

type
  TBlockFrame = class(TFrame)
    Rectangle: TRectangle;
    RectanglePrice: TRectangle;
    TextCurrentMax: TText;
    TextCurrentMin: TText;
  private
    FBlock: TBlock;
    procedure SetRectanglePrice;
    function GetPriceY(const APrice: Double): Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBlock(ABlock: TBlock);
    property Block: TBlock read FBlock;
  end;

implementation

{$R *.fmx}

{ TBlockFrame }

constructor TBlockFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlock := nil;
end;

destructor TBlockFrame.Destroy;
begin

  inherited;
end;

function TBlockFrame.GetPriceY(const APrice: Double): Single;
begin
  Result := Rectangle.Height * (FBlock.PriceMax - APrice)/(FBlock.PriceMax - FBlock.PriceMin);
end;

procedure TBlockFrame.SetBlock(ABlock: TBlock);
begin
  FBlock := ABlock;
  TextCurrentMax.Text := ABlock.PriceMax.ToString;
  TextCurrentMin.Text := ABlock.PriceMin.ToString;
  SetRectanglePrice;

  case FBlock.TypeBlock of
    tbNormal: RectanglePrice.Fill.Color := TAlphaColorRec.Goldenrod;
    tbUp: RectanglePrice.Fill.Color := TAlphaColorRec.Green;
    tbDown: RectanglePrice.Fill.Color := TAlphaColorRec.Red;
  end;

end;

procedure TBlockFrame.SetRectanglePrice;
var
  xTop, xLeft, xBottom, xWidth, xHeight: Single;
begin
  xTop := GetPriceY(FBlock.CurrentMax);
  xBottom := GetPriceY(FBlock.CurrentMin);
  xLeft := 0;
  xWidth := Self.Width;
  xHeight := xBottom - xTop;
  RectanglePrice.SetBounds(xLeft, xTop, xWidth, xHeight);
end;

end.
