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
  Lb.Status.Quik,
  Lb.Status.Bybit;


type
  TMainClientFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutBuy: TLayout;
    LayoutInfo: TLayout;
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
  StatusFrame.Status.OnParams := StatusFrameOnParams;
  StatusFrame.Status.OnNewCandel := StatusFrameOnNewCandel;
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
  xParam.FastRSI     := StatusFrame.Status.FastRSI;
  xParam.SlowRSI     := StatusFrame.Status.SlowRSI;
  xParam.Bid         := StatusFrame.Status.Bid;
  xParam.Ask         := StatusFrame.Status.Ask;
  xParam.Qty         := StatusFrame.Status.Position.Qty;
  xParam.Side        := StatusFrame.Status.Position.Side;
  xParam.IsNewCandel := StatusFrame.Status.IsNewCandel;
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
  if ParamApplication.IsNewCandel then
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
end;

procedure TMainClientFrame.EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
begin
  StatusFrame.Status.GetOperationTrade(
    TParamStatus.Create(
      ATradeParam.Side,
      ATradeParam.Price,
      ATradeParam.Qty,
      ATradeParam.TypeLine
    )
  );
end;

end.
