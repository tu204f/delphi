unit UnitMainClientFrame;

interface

{$I debug.inc}

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
  UnitStatusFrame,
  UnitOrderCategoryFrame,
  FMX.Objects,
  Lb.SysUtils,
  Lb.Bybit.SysUtils, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox,
  FMX.SpinBox;

type
  TMainClientFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutInfo: TLayout;
    LayoutBuy: TLayout;
  private
    FParam: TSituationParam;
    procedure InitOrderCategoryFrame;
  protected
    OrderCategoryBuy: TOrderCategoryFrame;
    OrderCategorySell: TOrderCategoryFrame;
    procedure StatusFrameOnParams(Sender: TObject);
    procedure EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
  public
    StatusFrame: TStatusFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TMainClientFrame }

constructor TMainClientFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitOrderCategoryFrame;
end;

destructor TMainClientFrame.Destroy;
begin
  FreeAndNil(OrderCategoryBuy);
  FreeAndNil(OrderCategorySell);
  inherited;
end;

procedure TMainClientFrame.InitOrderCategoryFrame;

  function _InitOrderCategoryFrame(const ALayout: TLayout): TOrderCategoryFrame;
  var
    xFrame: TOrderCategoryFrame;
  begin
    xFrame := TOrderCategoryFrame.Create(nil);
    xFrame.Parent := ALayout;
    xFrame.Align := TAlignLayout.Client;
    Result := xFrame;
  end;

  function _InitStatusFrame: TStatusFrame;
  var
    xFrame: TStatusFrame;
  begin
    xFrame := TStatusFrame.Create(nil);
    xFrame.Parent := LayoutInfo;
    xFrame.Align := TAlignLayout.Client;
    Result := xFrame;
  end;

begin
  OrderCategoryBuy  := _InitOrderCategoryFrame(LayoutBuy);
  OrderCategoryBuy.Side := TTypeSide.tsBuy;
  OrderCategoryBuy.OnEventSendTarde := EventSendTarde;
  OrderCategoryBuy.Color := TAlphaColorRec.Green;

  OrderCategorySell := _InitOrderCategoryFrame(LayoutSell);
  OrderCategorySell.Side := TTypeSide.tsSell;
  OrderCategorySell.OnEventSendTarde := EventSendTarde;
  OrderCategorySell.Color := TAlphaColorRec.Red;

  StatusFrame := _InitStatusFrame;
  StatusFrame.OnParams := StatusFrameOnParams;
end;

procedure TMainClientFrame.StatusFrameOnParams(Sender: TObject);
begin
  FParam.ValueRSI := StatusFrame.ValueRSI;
  FParam.Bid      := StatusFrame.Bid;
  FParam.Ask      := StatusFrame.Ask;
  FParam.Qty      := StatusFrame.Qty;
  FParam.Side     := StatusFrame.Side;

  OrderCategoryBuy.SetParams(FParam);
  OrderCategorySell.SetParams(FParam);
end;

procedure TMainClientFrame.EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
begin
  StatusFrame.SetOperationTrade(
    ATradeParam.Side,
    ATradeParam.Price,
    ATradeParam.Qty,
    ATradeParam.TypeLine
  );
end;

end.
