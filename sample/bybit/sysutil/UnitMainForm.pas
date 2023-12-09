unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  Lb.Bybit.ServerTime,
  Lb.Bybit.SysUtils;

//https://testnet.bybit.com/trade/usdt/BTCUSDT

type
  TMainForm = class(TForm)
    ButtonSelected: TButton;
    MemoResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonSelectedClick(Sender: TObject);
  private
    BybitServerTime: TBybitServerTime;
    BybitObject: TBybitHttpClient;
    function GetCreateBybitObject: TBybitHttpClient;
    procedure BybitObjectEventMessage(ASender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Hash,
  System.DateUtils,
  Lb.Bybit.Position,
  Lb.Bybit.Encryption,
  Lb.Bybit.InstrumentsInfo;

{ TMainForm }


(*
    private static String genGetSign(Map<String, Object> params) throws NoSuchAlgorithmException, InvalidKeyException {
        StringBuilder sb = genQueryStr(params);
        String queryStr = TIMESTAMP + API_KEY + RECV_WINDOW + sb;

        Mac sha256_HMAC = Mac.getInstance("HmacSHA256");
        SecretKeySpec secret_key = new SecretKeySpec(API_SECRET.getBytes(), "HmacSHA256");
        sha256_HMAC.init(secret_key);
        return bytesToHex(sha256_HMAC.doFinal(queryStr.getBytes()));
    }
*)

function TMainForm.GetCreateBybitObject: TBybitHttpClient;
var
  xBybitObject: TBybitHttpClient;
begin
  xBybitObject := TBybitHttpClient.Create;
  //xBybitObject := TBybitPosition.Create;
  Result := xBybitObject;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Запрос на сервер Bybit';
  BybitObject := GetCreateBybitObject;
  if Assigned(BybitObject) then
  begin
    BybitObject.OnEventMessage := BybitObjectEventMessage;
    BybitObject.OnEventException := BybitObjectEventMessage;
  end;

  BybitServerTime := TBybitServerTime.Create;
  BybitServerTime.Selected(100);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(BybitServerTime);
  if Assigned(BybitObject) then
    FreeAndNil(BybitObject);
end;

procedure TMainForm.ButtonSelectedClick(Sender: TObject);
var
  xEncryption: TEncryption;
begin
  MemoResult.Lines.Add(BybitServerTime.TimeSecond + ' :: ' + BybitServerTime.TimeNano);

  xEncryption := TEncryption.Create;

  xEncryption.ApiKey    := 'IYokQRNi1KjdlQ34vT';
  xEncryption.ApiSecret := 'cRQVjujmbZOAc4yIeoPTR2izhTbQPlkPgsGN';
  xEncryption.Timestamp := BybitServerTime.TimeSecond + '000';

  if Assigned(BybitObject) then
  begin
    with BybitObject do
    begin
      ModuleParam.TypeHttp := TTypeHttp.thGet;
      ModuleParam.Module := '/v5/position/list';

      with ModuleParam.Params do
      begin
        SetParam('category',GetStrToTypeCategory(TTypeCategory.tcLinear));
        SetParam('symbol','BTCUSDT');
        // SetParam('interval',GetStrToTypeInterval(TTypeInterval.ti_5));
        // SetParam('limit',IntToStr(1000));
        // SetParam('status',GetStrToTypeStatus(TTypeStatus.tsTrading));
        // baseCoin
        // limit
        // cursor
      end;
      xEncryption.QueryBody := ModuleParam.Query;

      with ModuleParam.Headers do
      begin
        Values['X-BAPI-API-KEY']     := xEncryption.ApiKey;
        Values['X-BAPI-TIMESTAMP']   := xEncryption.Timestamp;
        Values['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
        Values['X-BAPI-SIGN-TYPE']   := '2';
        Values['X-BAPI-SIGN']        := xEncryption.Signature;
      end;
    end;
    BybitObject.Selected(0);
  end;

  FreeAndNil(xEncryption);
end;

procedure TMainForm.BybitObjectEventMessage(ASender: TObject);
begin
  MemoResult.Lines.Add(BybitObject.StatusCode.ToString);
  MemoResult.Lines.Add(BybitObject.ValueMessage);
//    TBybitServerTime(BybitObject).TimeSecond + ' ' +
//    TBybitServerTime(BybitObject).TimeNano + ' ' +
//    DateTimeToStr(
//      TBybitServerTime(BybitObject).DateTimeServer
//    );
end;

end.
