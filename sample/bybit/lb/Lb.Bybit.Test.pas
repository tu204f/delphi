unit Lb.Bybit.Test;

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
  FMX.Types,
  Lb.Bybit.ServerTime,
  Lb.Bybit.SysUtils;

type
  ///<summary>Объект проверки соединение</summary>
  TBybitTest = class(TObject)
  private
    FOnEventSing: TNotifyEvent;
    FTimeOut: Integer;
    FTimer: TTimer;
    FServerTime: TBybitServerTime;
    procedure TimerUpData(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    ///<summary>Задержка времени</summary>
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property OnEventSing: TNotifyEvent write FOnEventSing;
  end;

implementation

{ TBybitTest }

constructor TBybitTest.Create;
begin
  FTimeOut := 5000;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;
  FTimer.OnTimer := TimerUpData;

  FServerTime := TBybitServerTime.Create;
end;

destructor TBybitTest.Destroy;
begin
  FreeAndNil(FServerTime);
  FreeAndNil(FTimer);
  inherited;
end;

procedure TBybitTest.Start;
begin
  if not FTimer.Enabled then
  begin
    FTimer.Enabled := True;
    FServerTime.Start(1000);
  end;
end;

procedure TBybitTest.TimerUpData(Sender: TObject);
var
  xDelta: Integer;
  xNowTime: Int64;
begin
  xNowTime := GetNow;
  xDelta := StrToInt64Def(FServerTime.TimeSecond,0) * 1000 - xNowTime;
  xDelta := Abs(xDelta);
  if xDelta > FTimeOut then
  begin
    if Assigned(FOnEventSing) then
      FOnEventSing(Self);
  end;
end;

end.
