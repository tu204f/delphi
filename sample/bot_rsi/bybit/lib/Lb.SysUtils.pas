unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  Lb.Bybit.SysUtils;


type
  IMainApp = interface
    ///<summary>
    /// Событие закрытие TabControl, открываем Main
    ///</summary>
    procedure EventCloseTabControl;
  end;

  ///<summary>
  /// Параметры работы программы
  ///</summary>
  TParamApplication = class(TObject)
  public const
    SECTION_PARAM    = 'param';

    IDENT_SYMBLE     = 'symble';
    IDENT_API_KEY    = 'api_key';
    IDENT_API_SECRET = 'api_secret';
    IDENT_CATEGORY   = 'category';
    IDENT_INTERVAL   = 'interval';

  private
    FSymble: String;
    FApiKey: String;
    FApiSecret: String;
    FCategory: TTypeCategory;
    FInterval: TTypeInterval;
  protected
    function GetFileName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  public
    ///<summary>
    /// Торгуемый символ
    ///</summary>
    property Symble: String read FSymble write FSymble;
    ///<summary>
    /// API Ключ (пуличный)
    ///</summary>
    property ApiKey: String read FApiKey write FApiKey;
    ///<summary>
    /// API секрет (секретный ключ)
    ///</summary>
    property ApiSecret: String read FApiSecret write FApiSecret;
    ///<summary>
    /// Категория инструмента торгуемого на bybit: IDENT_CATEGORY
    ///</summary>
    property Category: TTypeCategory read FCategory write FCategory;
    ///<summary>
    /// Интервал — размер свечи загружаемой
    ///</summary>
    property Interval: TTypeInterval read FInterval write FInterval;
  end;

function ParamApplication: TParamApplication;

implementation

uses
  System.IniFiles;

var
  localParamApplication: TParamApplication = nil;

function ParamApplication: TParamApplication;
begin
  if not Assigned(localParamApplication) then
  begin
    localParamApplication := TParamApplication.Create;
    localParamApplication.Load;
  end;
  Result := localParamApplication;
end;

procedure SetWriteStringParam(ASource: TStrings; ASection, AIdentm, AValue: String);
begin
  ASource.Add(ASection + '.' + AIdentm + '=' + AValue);
end;

function GetReadStringParam(ASource: TStrings; ASection, AIdentm, ADefault: String): String;
var
  xValue: String;
begin
  xValue := ASource.Values[ASection + '.' + AIdentm];
  if xValue.IsEmpty then
    Result := ADefault
  else
    Result := xValue;
end;

{ TParamApplication }

constructor TParamApplication.Create;
begin

end;

destructor TParamApplication.Destroy;
begin

  inherited;
end;

function TParamApplication.GetFileName: String;
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + 'config.ini';
  Result := xFileName;
end;

procedure TParamApplication.Load;
var
  xIni: TIniFile;
begin
  var xFN := GetFileName;
  xIni := TIniFile.Create(xFN);
  try
    if FileExists(xFN) then
    begin
      FSymble    := xIni.ReadString(SECTION_PARAM,IDENT_SYMBLE,'');
      FApiKey    := xIni.ReadString(SECTION_PARAM,IDENT_API_KEY,'');
      FApiSecret := xIni.ReadString(SECTION_PARAM,IDENT_API_SECRET,'');
      FCategory  := TTypeCategory(xIni.ReadInteger(SECTION_PARAM,IDENT_CATEGORY,1));
      FInterval  := TTypeInterval(xIni.ReadInteger(SECTION_PARAM,IDENT_INTERVAL,1));
    end;
  finally
    FreeAndNil(xIni);
  end;
end;

procedure TParamApplication.Save;
var
  xIni: TIniFile;
begin
  var xFN := GetFileName;
  xIni := TIniFile.Create(xFN);
  try
    xIni.WriteString(SECTION_PARAM,IDENT_SYMBLE,FSymble);
    xIni.WriteString(SECTION_PARAM,IDENT_API_KEY,FApiKey);
    xIni.WriteString(SECTION_PARAM,IDENT_API_SECRET,FApiSecret);
    xIni.WriteInteger(SECTION_PARAM,IDENT_CATEGORY,Integer(FCategory));
    xIni.WriteInteger(SECTION_PARAM,IDENT_INTERVAL,Integer(FInterval));
  finally
    FreeAndNil(xIni);
  end;
end;

initialization

finalization
  if Assigned(localParamApplication) then
    FreeAndNil(localParamApplication);

end.
