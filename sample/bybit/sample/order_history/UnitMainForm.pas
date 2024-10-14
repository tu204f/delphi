unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  Lb.Bybit.OrderHistory, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  Lb.Bybit.TradeHistory,
  Lb.Bybit.SysUtils;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure SetLog(S: String);
    procedure OrderHistoryOnEventEndLoading(Sender: TObject);
    procedure OrderHistoryOnEventException(Sender: TObject);
    procedure TradeHistoryOnEventEndLoading(Sender: TObject);
    procedure TradeHistoryOnEventException(Sender: TObject);
  public
    OrderHistory: TBybitOrderHistory;
    TradeHistory: TBybitTradeHistory;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

{ TForm4 }



constructor TForm4.Create(AOwner: TComponent);
begin
  inherited;
  OrderHistory := TBybitOrderHistory.Create;
  OrderHistory.OnEventEndLoading := OrderHistoryOnEventEndLoading;
  OrderHistory.OnEventException := OrderHistoryOnEventException;

  TradeHistory := TBybitTradeHistory.Create;
  TradeHistory.OnEventEndLoading := TradeHistoryOnEventEndLoading;
  TradeHistory.OnEventException := TradeHistoryOnEventException;
end;

destructor TForm4.Destroy;
begin
  FreeAndNil(TradeHistory);
  FreeAndNil(OrderHistory);
  inherited;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  OrderHistory.Category := TTypeCategory.tcLinear;
  OrderHistory.Symbol := 'ETHUSDT';
  OrderHistory.SetEncryption(
    '3bvDxJnKzjkIg8y0RV',//ApiKey,
    'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ'//ParamPlatform.ApiSecret
  );
  OrderHistory.Selected;


  TradeHistory.Category := TTypeCategory.tcLinear;
  TradeHistory.Symbol := 'ETHUSDT';
  TradeHistory.SetEncryption(
    '3bvDxJnKzjkIg8y0RV',//ApiKey,
    'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ'//ParamPlatform.ApiSecret
  );
  TradeHistory.Selected;

end;

procedure TForm4.OrderHistoryOnEventEndLoading(Sender: TObject);
var
  i, iCount: Integer;
  xOrderHistory: TOrderHistory;
begin
  SetLog('** *********************************************************');
  iCount := OrderHistory.OrderHistorys.Count;
  SetLog('OrderHistoryOnEventEndLoading: ' + iCount.ToString);
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xOrderHistory := OrderHistory.OrderHistorys[i];

      SetLog(
        xOrderHistory.orderId + ';' +
        xOrderHistory.price + '; ' +
        xOrderHistory.qty + '; ' +
        xOrderHistory.side
      );

    end;
end;

procedure TForm4.OrderHistoryOnEventException(Sender: TObject);
begin
  SetLog(
    OrderHistory.StatusCode.ToString + ' ' +
    OrderHistory.ValueMessage
  );
end;

procedure TForm4.TradeHistoryOnEventEndLoading(Sender: TObject);
var
  i, iCount: Integer;
  xTradeHistory: TTradeHistory;
begin
  SetLog('** *********************************************************');
  iCount := TradeHistory.TradeHistorys.Count;
  SetLog('TradeHistoryOnEventEndLoading: ' + iCount.ToString);
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTradeHistory := TradeHistory.TradeHistorys[i];

      SetLog(
        xTradeHistory.orderId + ';' +
        xTradeHistory.execTime + ';' +
        xTradeHistory.execPrice + '; ' +
        xTradeHistory.execQty + '; ' +
        xTradeHistory.side
      );

    end;
end;

procedure TForm4.TradeHistoryOnEventException(Sender: TObject);
begin
  SetLog(
    TradeHistory.StatusCode.ToString + ' ' +
    TradeHistory.ValueMessage
  );
end;

procedure TForm4.SetLog(S: String);
begin
  Memo1.Lines.Add(S);
end;

end.
