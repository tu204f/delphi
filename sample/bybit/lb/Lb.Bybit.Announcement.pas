unit Lb.Bybit.Announcement;

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
  ///<summary>�������� ����������</summary>
  ///<remarks>��������� �� �����</remarks>
  TBybitAnnouncement = class(TBybitHttpClient)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TBybitAnnouncement }

constructor TBybitAnnouncement.Create;
begin
  inherited Create;
  ModuleParam.TypeHttp := TTypeHttp.thGet;
  ModuleParam.Module := '/v5/announcements/index';
  with ModuleParam.Params do
  begin
    // https://bybit-exchange.github.io/docs/v5/enum#locale
    SetParam('locale','ru-RU');
    // https://bybit-exchange.github.io/docs/v5/enum#announcementtype
    //SetParam('type','new_crypto');
    // https://bybit-exchange.github.io/docs/v5/enum#announcementtag
    //SetParam('tag','Spot');
    // page
    // limit
  end;
end;

destructor TBybitAnnouncement.Destroy;
begin

  inherited;
end;

end.
