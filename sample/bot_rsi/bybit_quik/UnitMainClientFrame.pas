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
  Lb.SysUtils,
  Lb.Status,
  UnitOrderUsersFrame;


type
  TMainClientFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutBuy: TLayout;
    LayoutInfo: TLayout;
    LayoutButtomBuySell: TLayout;
  private
    procedure InitOrderCategoryFrame;
  protected
    OrderCategoryBuy: TOrderCategoryFrame;
    OrderCategorySell: TOrderCategoryFrame;
    procedure StatusFrameOnParams(Sender: TObject);
    procedure StatusFrameOnNewCandel(Sender: TObject);
    procedure EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
  public
    StatusFrame: TStatusFrame;
    OrderUsersFrame: TOrderUsersFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  UnitLogForm,
  Lb.Logger;

{ TMainClientFrame }

constructor TMainClientFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitOrderCategoryFrame;

end;

destructor TMainClientFrame.Destroy;
begin
  FreeAndNil(OrderUsersFrame);
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

  OrderUsersFrame := TOrderUsersFrame.Create(nil);
  OrderUsersFrame.OnEventSendTarde := EventSendTarde;
  OrderUsersFrame.Align := TAlignLayout.Client;
  OrderUsersFrame.Parent := LayoutButtomBuySell;

  StatusFrame := _InitStatusFrame;
  Status.OnParams := StatusFrameOnParams;
  Status.OnNewCandel := StatusFrameOnNewCandel;
end;

procedure TMainClientFrame.StatusFrameOnNewCandel(Sender: TObject);
begin
{$IFDEF DBG_TRD}
  TLogger.LogTree(0,'procedure TMainClientFrame.StatusFrameOnNewCandel');
{$ENDIF}
end;

procedure TMainClientFrame.StatusFrameOnParams(Sender: TObject);
var
  xParam: TSituationParam;
begin
  xParam.FastRSI     := Status.FastRSI;
  xParam.SlowRSI     := Status.SlowRSI;
  xParam.Bid         := Status.Bid;
  xParam.Ask         := Status.Ask;
  xParam.Qty         := Status.Position.Qty;
  xParam.Side        := Status.Position.Side;
  xParam.IsNewCandel := Status.IsNewCandel;
{$IFDEF DBG_TRD}
  TLogger.LogTree(0,'procedure TMainClientFrame.StatusFrameOnParams');
  TLogger.LogTreeText(3,'>> FastRSI = ' + xParam.FastRSI.ToString);
  TLogger.LogTreeText(3,'>> SlowRSI = ' + xParam.SlowRSI.ToString);
  TLogger.LogTreeText(3,'>> Bid = ' + xParam.Bid.ToString);
  TLogger.LogTreeText(3,'>> Ask = ' + xParam.Ask.ToString);
  TLogger.LogTreeText(3,'>> Qty = ' + xParam.Qty.ToString);
  TLogger.LogTreeText(3,'>> Side = ' + GetStrToTypeSide(xParam.Side));
  if xParam.IsNewCandel then
    TLogger.LogTreeText(3,'>> IsNewCandel = True')
  else
    TLogger.LogTreeText(3,'>> IsNewCandel = False');
{$ENDIF}
  // -------------------------------------------------------------------------
  // Проверяем новая свечу
  if ParamPlatform.IsNewCandel then
  begin
    if xParam.IsNewCandel then
    begin
      OrderCategoryBuy.SetParams(xParam);
      OrderCategorySell.SetParams(xParam);
    end;
  end
  else
  begin
    OrderCategoryBuy.SetParams(xParam);
    OrderCategorySell.SetParams(xParam);
  end;

  OrderUsersFrame.SetValueParam(xParam);
end;

procedure TMainClientFrame.EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
begin
  Status.GetOperationTrade(
    TParamStatus.Create(
      ATradeParam.Side,
      ATradeParam.Price,
      ATradeParam.Qty,
      ATradeParam.TypeLine
    )
  );
end;

end.
