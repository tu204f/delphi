unit Lb.Bybit;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  Lb.Bybit.ServerTime,
  Lb.Bybit.SysUtils;

type
  ///<summary>Объект установки соединение</summary>
  TBybitConnection = class(TObject)
  private
    FActive: Boolean;
    FHost: String;
    FServerTime: TBybitServerTime;
    procedure ServerTimeOnEventMessage(Sender: TObject);
    procedure ServerTimeOnEventException(Sender: TObject);
    procedure SetActive(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connected;
    procedure Disconnected;
    property Active: Boolean read FActive write SetActive;
    property Host: String read FHost write FHost;
  end;

implementation

{ TBybitConnection }

constructor TBybitConnection.Create;
begin
  FHost := '';
  FServerTime := TBybitServerTime.Create;
  FServerTime.OnEventMessage   := ServerTimeOnEventMessage;
  FServerTime.OnEventException := ServerTimeOnEventException;
end;

destructor TBybitConnection.Destroy;
begin
  FreeAndNil(FServerTime);
  inherited;
end;

procedure TBybitConnection.Connected;
begin
  if not FActive then
  begin
    FActive := True;
    FServerTime.Selected(1000);
  end;
end;

procedure TBybitConnection.Disconnected;
begin
  if FActive then
  begin
    FActive := False;
    FServerTime.Stop;
  end;
end;

procedure TBybitConnection.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if FActive then
    Disconnected
  else
    Connected;
end;

procedure TBybitConnection.ServerTimeOnEventException(Sender: TObject);
begin
  // Была ошибка соединение свервером
  FActive := False;
end;

procedure TBybitConnection.ServerTimeOnEventMessage(Sender: TObject);
begin
  // Соединение поддерживается
end;

end.
