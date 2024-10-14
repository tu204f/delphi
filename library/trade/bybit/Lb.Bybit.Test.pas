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
    FTimeSecond: Int64;
    FOnEventSing: TNotifyEvent;
    FOnEventTimer: TNotifyEvent;
    FTimeOut: Integer;
    FTimer: TTimer;
    FServerTime: TBybitServerTime;
    procedure TimerUpData(Sender: TObject);
    function GetTimeSecond: TDateTime;
  protected
    procedure DoEventSing;
    procedure DoEventTimer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    ///<summary>Задержка времени</summary>
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property OnEventSing: TNotifyEvent write FOnEventSing;
    property OnEventTimer: TNotifyEvent write FOnEventTimer;
    property TimeSecond: TDateTime read GetTimeSecond;
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

procedure TBybitTest.DoEventSing;
begin
  if Assigned(FOnEventSing) then
    FOnEventSing(Self);
end;

procedure TBybitTest.DoEventTimer;
begin
  if Assigned(FOnEventTimer) then
    FOnEventTimer(Self);
end;

function TBybitTest.GetTimeSecond: TDateTime;
begin
  Result := UnixToDateTime(FTimeSecond);
end;

procedure TBybitTest.Start;
begin
  if not FTimer.Enabled then
  begin
    FTimer.Enabled := True;
    FServerTime.Start(1000);
  end;
end;

procedure TBybitTest.Stop;
begin
  FTimer.Enabled := False;
  FServerTime.Stop;
end;

procedure TBybitTest.TimerUpData(Sender: TObject);
var
  xDelta: Integer;
  xNowTime: Int64;
begin
  FTimeSecond := StrToInt64Def(FServerTime.TimeSecond,0);
  xNowTime := GetNow;
  xDelta := FTimeSecond * 1000 - xNowTime;
  xDelta := Abs(xDelta);
  if xDelta > FTimeOut then
    DoEventSing;
  DoEventTimer;
end;

end.
