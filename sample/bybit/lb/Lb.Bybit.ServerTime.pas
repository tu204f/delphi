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
  TBybitServerTime = class(TBybitObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TBybitServerTime }

constructor TBybitServerTime.Create;
begin
  inherited Create;
  Self.Module := '/v5/market/time';
end;

destructor TBybitServerTime.Destroy;
begin

  inherited;
end;

end.
