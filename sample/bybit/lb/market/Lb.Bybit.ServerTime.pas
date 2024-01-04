unit Lb.Bybit.ServerTime;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.Bybit.SysUtils;

type
  ///<summary>Get Bybit Server Time - время на сервере</summary>
  TBybitServerTime = class(TBybitHttpClient)
  private
    FTimeSecond: String;
    FTimeNano: String;
    function GetDateTimeServer: TDateTime;
  protected
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property TimeSecond: String read FTimeSecond;
    property TimeNano: String read FTimeNano;
    property DateTimeServer: TDateTime read GetDateTimeServer;
  end;

implementation

{ TBybitServerTime }

constructor TBybitServerTime.Create;
begin
  inherited Create;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/market/time';
end;

destructor TBybitServerTime.Destroy;
begin
  Self.Stop;
  inherited;
end;

procedure TBybitServerTime.DoEventParser;
begin
  try
    FTimeSecond := Response.ResultObject.Values['timeSecond'].Value;
    FTimeNano   := Response.ResultObject.Values['timeNano'].Value;
  except
    raise EAbort.Create('Error Message: Ошибка парсинга');
  end;
end;

function TBybitServerTime.GetDateTimeServer: TDateTime;
var
  xValue: TDateTime;
begin
  xValue := UnixToDateTime(StrToUInt64Def(FTimeSecond,0));
  Result := xValue;
end;

end.
