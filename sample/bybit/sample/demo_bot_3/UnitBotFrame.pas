unit UnitBotFrame;

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Edit,
  FMX.Layouts,
  FMX.Menus,
  Lb.Bybit.SysUtils,
  Lb.Bybit.RealTime,
  Lb.Bybit.Trade,
  UnitIndicatorFrame,
  UnitOrderFrame,
  UnitOrderBookFrame,
  FMX.Objects;

type
  TBotFrame = class(TFrame)
    GridLayout: TGridPanelLayout;
    LayoutBuy: TLayout;
    LayoutSell: TLayout;
    LayoutTop: TLayout;
    TextInfo1: TText;
    TextTime: TText;
    TextInfo2: TText;
    TextPrice: TText;
    TextInfo3: TText;
    TextRSI: TText;
    LayoutOrderBook: TLayout;
  private
  protected
    OrderBook: TOrderBookFrame;
    OrderBuy: TOrderFrame;
    OrderSell: TOrderFrame;
  public
    //Indicator: TIndicator;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function UpDataTimer: Boolean;
  end;

implementation

{$R *.fmx}

{ TBotFrame }

constructor TBotFrame.Create(AOwner: TComponent);
begin
  inherited;
  //Indicator := nil;

  OrderBuy  := TOrderFrame.Create(nil);
  OrderBuy.Parent := LayoutBuy;
  OrderBuy.Align := TAlignLayout.Client;
  OrderBuy.Side := TTypeSide.tsBuy;

  OrderSell := TOrderFrame.Create(nil);
  OrderSell.Parent := LayoutSell;
  OrderSell.Align := TAlignLayout.Client;
  OrderSell.Side := TTypeSide.tsSell;

  OrderBook := TOrderBookFrame.Create(nil);
  OrderBook.Parent := LayoutOrderBook;
  OrderBook.Align := TAlignLayout.Client;

end;

destructor TBotFrame.Destroy;
begin
  FreeAndNil(OrderBook);
  FreeAndNil(OrderBuy);
  FreeAndNil(OrderSell);
  inherited;
end;

function TBotFrame.UpDataTimer: Boolean;

  procedure _UpData(const AOrder: TOrderFrame);
  begin
    //AOrder.Indicator := Indicator;
    AOrder.SetUpData;
  end;

begin
  Result := False;
//  if Assigned(Indicator) then
//  begin
//
//    TextTime.Text  := DateToStr(Indicator.CurrentCandel.DateTime);
//    TextPrice.Text := FloatToStr(Indicator.CurrentCandel.Close);
//    TextRSI.Text   := FloatToStr(Indicator.RSI.Current.Value);
//
//    _UpData(OrderBuy);
//    _UpData(OrderSell);
//
//    OrderBook.SetUpData;
//
//    Result := True;
//  end;
end;

end.
