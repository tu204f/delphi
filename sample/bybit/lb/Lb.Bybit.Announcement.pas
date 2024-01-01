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
  ///<summary>Получить объявление</summary>
  ///<remarks>Сообщение от бирже</remarks>
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
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/announcements/index';
  with BybitModule.Params do
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
