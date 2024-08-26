unit UnitMainClientFrame;

interface

{$i platform.inc}

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
  Lb.SysUtils;


type
  TMainClientFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutInfo: TLayout;
    LayoutBuy: TLayout;
  private
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
  OrderCategoryBuy.Side := TQBTypeSide.tsBuy;
  OrderCategoryBuy.OnEventSendTarde := EventSendTarde;

  OrderCategorySell := _InitOrderCategoryFrame(LayoutSell);
  OrderCategorySell.Side := TQBTypeSide.tsSell;
  OrderCategorySell.OnEventSendTarde := EventSendTarde;

  StatusFrame := _InitStatusFrame;
  StatusFrame.OnParams := StatusFrameOnParams;
end;

procedure TMainClientFrame.StatusFrameOnParams(Sender: TObject);
var
  xParam: TSituationParam;
begin
  xParam.ValueRSI := StatusFrame.ValueRSI;
  xParam.Bid      := StatusFrame.Bid;
  xParam.Ask      := StatusFrame.Ask;
  xParam.Qty      := StatusFrame.Qty;
  xParam.Side     := StatusFrame.Side;

  OrderCategoryBuy.SetParams(xParam);
  OrderCategorySell.SetParams(xParam);
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
