unit UnitOrderBookRowFrame;

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
  FMX.Layouts,
  FMX.Objects,
  Lb.Bybit.SysUtils,
  Lb.Bybit.OrderBook;

type
  TOrderBookRowFrame = class(TFrame)
    GridLayout: TGridPanelLayout;
    LayoutPrice: TLayout;
    LayoutQuantity: TLayout;
    RectangleFon: TRectangle;
    TextPrice: TText;
    TextQuantity: TText;
  private
    FOrderRow: TOrderRow;
    procedure SetOrderRow(const Value: TOrderRow);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OrderRow: TOrderRow read FOrderRow write SetOrderRow;
  end;

implementation

{$R *.fmx}

{ TOrderBookRowFrame }

constructor TOrderBookRowFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TOrderBookRowFrame.Destroy;
begin

  inherited;
end;

procedure TOrderBookRowFrame.SetOrderRow(const Value: TOrderRow);
begin
  {todo: возможность учитовать точность}
  FOrderRow := Value;
  TextPrice.Text    := FOrderRow.Price.ToString;
  TextQuantity.Text := FOrderRow.Quantity.ToString;
  case FOrderRow.BuySell of
    tsBuy : RectangleFon.Fill.Color := TAlphaColorRec.Green;
    tsSell: RectangleFon.Fill.Color := TAlphaColorRec.Red;
  end;
end;

end.
