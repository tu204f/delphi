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
  Lb.Bybit.Position,
  Lb.Bybit.ServerTime,
  Lb.Bybit.SysUtils;

//https://testnet.bybit.com/trade/usdt/BTCUSDT

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonServerTime: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonServerTimeClick(Sender: TObject);
  private
    Position: TBybitPosition;
    BybitServerTime: TBybitServerTime;
    BybitObject: TBybitHttpClient;
  protected
    procedure BybitServerTimeOnEventBeginLoading(Sender: TObject);
    procedure BybitServerTimeOnEventEndLoading(Sender: TObject);
    procedure BybitServerTimeOnEventMessage(Sender: TObject);
    procedure BybitServerTimeOnEventException(Sender: TObject);
  public
    procedure SetLog(S: String = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Hash,
  System.DateUtils,
  Lb.Bybit.Encryption,
  Lb.Bybit.InstrumentsInfo;

{ TMainForm }

procedure TMainForm.SetLog(S: String);
begin
  MemoResult.Lines.Add(S);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Position := nil;
  BybitObject  := nil;
  Self.Caption := 'Запрос на сервер Bybit';

  BybitServerTime := TBybitServerTime.Create;
  BybitServerTime.OnEventBeginLoading := BybitServerTimeOnEventBeginLoading;
  BybitServerTime.OnEventEndLoading   := BybitServerTimeOnEventEndLoading;
  BybitServerTime.OnEventMessage      := BybitServerTimeOnEventMessage;
  BybitServerTime.OnEventException    := BybitServerTimeOnEventException;

end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(BybitServerTime);
  if Assigned(BybitObject) then
    FreeAndNil(BybitObject);
  MemoResult.Lines.Clear;
end;

procedure TMainForm.ButtonServerTimeClick(Sender: TObject);
begin
  BybitServerTime.Start(1000);
end;

procedure TMainForm.BybitServerTimeOnEventBeginLoading(Sender: TObject);
begin
  // Начало загрузки
  SetLog('*******************************************************************');
  SetLog('BybitServerTimeOnEventBeginLoading:');
end;

procedure TMainForm.BybitServerTimeOnEventEndLoading(Sender: TObject);
begin
  // Конец парсинга сообщений
  SetLog('BybitServerTimeOnEventEndLoading:');
end;

procedure TMainForm.BybitServerTimeOnEventMessage(Sender: TObject);
begin
  // Получение сообщение
  SetLog('BybitServerTimeOnEventMessage:');
  SetLog(BybitServerTime.StatusCode.ToString);
  SetLog(BybitServerTime.ValueMessage);
end;

procedure TMainForm.BybitServerTimeOnEventException(Sender: TObject);
begin
  // Сообщение обошибки
  SetLog('BybitServerTimeOnEventException:');
  SetLog(BybitServerTime.StatusCode.ToString);
  SetLog(BybitServerTime.ValueMessage);
end;

end.
