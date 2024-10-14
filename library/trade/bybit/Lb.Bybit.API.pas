unit Lb.Bybit.API;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.JSON;

type
  ///<summary><summary>
  TBybitHttpClientAPI = class(TObject)
  private
    FURL: String;
  protected
    procedure SetSelected;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property URL: String read FURL write FURL;
  end;

implementation

{ TBybitHttpClientAPI }

constructor TBybitHttpClientAPI.Create;
begin

end;

destructor TBybitHttpClientAPI.Destroy;
begin

  inherited;
end;

procedure TBybitHttpClientAPI.SetSelected;
begin

end;

end.
