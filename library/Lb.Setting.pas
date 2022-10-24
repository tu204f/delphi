unit Lb.Setting;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
{$IFDEF MSWINDOWS}
  Vcl.ExtCtrls;
{$ELSE}
  FMX.Types;
{$ENDIF}


type
  TSetting = record
  private
    class function GetEncoding: TEncoding; static;
  public
    class function ReadString(const AParam: String; ADefault: String = ''): String; static;
    class function ReadInteger(const AParam: String; Default: Integer = 0): Integer; static;
    class function ReadBool(const AParam: String; Default: Boolean = False): Boolean; static;
    class function ReadDate(const AParam: String; Default: TDateTime): TDateTime; static;
    class function ReadDateTime(const AParam: String; Default: TDateTime): TDateTime; static;
    class function ReadFloat(const AParam: String; Default: Double): Double; static;
    class function ReadTime(const AParam: String; Default: TDateTime): TDateTime; static;
    class procedure WriteString(const AParam: String; AValue: String); static;
    class procedure WriteInteger(const AParam: String; AValue: Integer); static;
    class procedure WriteBool(const AParam: String; Value: Boolean); static;
    class procedure WriteDate(const AParam: String; Value: TDateTime); static;
    class procedure WriteDateTime(const AParam: String; Value: TDateTime); static;
    class procedure WriteFloat(const AParam: String;  Value: Double); static;
    class procedure WriteTime(const AParam: String; Value: TDateTime); static;
    class procedure SetPost; static;
  end;

type
  {TODO: Доработать объетк для с можеством файлов}

  /// <summary>
  /// Создаем в памяти локальную временную версию
  /// </summary>
  /// <remarks>
  ///  <para>Пока поумолчанию работает в одним файлом</para>
  ///  <para>config.ini</para>
  ///  <para>Раз в 10 секунд производит сохранение данных, на диске</para>
  /// </remarks>
  TGlobalIniFiles = class(TObject)
  private const
    FILE_NAME_CONFIG = 'config.ini';
  private
    FActiveLock: Boolean;
    FMemIniFile: TMemIniFile;
    FTimer: TTimer;
    procedure EventTimer(Sender: TObject);
  protected
    procedure SetLock;
    procedure SetUnLock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPost;
    function ReadString(const AParam: String; ADefault: String = ''): String;
    procedure WriteString(const AParam: String; AValue: String);
    property ActiveLock: Boolean read FActiveLock;
    property IniFile: TMemIniFile read FMemIniFile;
  public
    class function GetIniFiles: TGlobalIniFiles;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Vcl.Forms;
{$ELSE}
  FMX.Forms;
{$ENDIF}

type
  TParam = record
    FileName: String;
    Section: String;
    Ident: String;
  private
    procedure SetParam(const Value: String);
  public
    property Param: String write SetParam;
  end;

{ TParam }

procedure TParam.SetParam(const Value: String);
var
  xC: Char;
  tmpS, xPath: String;
  xStr: TStrings;
begin
  tmpS := '';
  xPath := ExtractFilePath(ParamStr(0));
  xStr := TStringList.Create;
  try
    for xC in Value do
    begin
      if xC = '.' then
      begin
        if not tmpS.IsEmpty then
        begin
          xStr.Add(tmpS);
          tmpS := '';
        end;
      end
      else
        tmpS := tmpS + xC;
        //if CharInSet(xC,['A'..'Z','a'..'z']) then
    end;
    xStr.Add(tmpS);

    case xStr.Count of
      1: begin
        FileName := xPath + 'config.ini';
        Section := 'sys';
        Ident := xStr[0];
      end;
      2: begin
        FileName := xPath + 'config.ini';
        Section := xStr[0];
        Ident := xStr[1];
      end;
      3: begin
        FileName := xPath + xStr[0] + '.ini';
        Section := xStr[1];
        Ident := xStr[2];
      end;
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

var
  GlobalIniFiles: TGlobalIniFiles = nil;

{ TGlobalIniFiles }

constructor TGlobalIniFiles.Create;
var
  xPath: String;
begin
  FActiveLock := False;
  xPath := ExtractFilePath(ParamStr(0)) + FILE_NAME_CONFIG;
  FMemIniFile := TMemIniFile.Create(xPath,TSetting.GetEncoding);
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := True;
  FTimer.OnTimer := EventTimer;
  FTimer.Interval := 1000;
end;

destructor TGlobalIniFiles.Destroy;
begin
  FreeAndNil(FTimer);
  FMemIniFile.UpdateFile;
  FreeAndNil(FMemIniFile);
  inherited;
end;

function TGlobalIniFiles.ReadString(const AParam: String; ADefault: String): String;
var
  xParam: TParam;
  xS: String;
  xInd: Integer;
begin
  // Запускаем бисконечный цикл, пока сохроняется файл
  while Self.ActiveLock do
    Application.ProcessMessages;
  xParam.Param := AParam;
  xS := FMemIniFile.ReadString(xParam.Section,xParam.Ident,ADefault);
  xInd := Pos('#',xS);
  if xInd > 0 then
    Result := Trim(Copy(xS,1,xInd - 1))
  else
    Result := xS;
end;

procedure TGlobalIniFiles.WriteString(const AParam: String; AValue: String);
var
  xParam: TParam;
begin
  if not Assigned(GlobalIniFiles) then
    GlobalIniFiles := TGlobalIniFiles.Create;
  // Запускаем бисконечный цикл, пока сохроняется файл
  while Self.ActiveLock do
    Application.ProcessMessages;
  xParam.Param := AParam;
  FMemIniFile.WriteString(xParam.Section,xParam.Ident,AValue);
end;

procedure TGlobalIniFiles.SetLock;
begin
  FActiveLock := True;
end;

procedure TGlobalIniFiles.SetPost;
begin
  SetLock;
  try
    try
      {$IFDEF WIN_XP}
      TLogger.LogForm('win_xp','global_ini_files.set_post');
      {$ENDIF}
      FMemIniFile.UpdateFile;
    except
      on E : Exception do

        //TLogger.LogForm('win_xp','global_ini_files.set_post ' + E.ClassName + ' ' + E.Message);
    end;
  finally
    SetUnLock;
  end;
end;

procedure TGlobalIniFiles.SetUnLock;
begin
  FActiveLock := False;
end;

procedure TGlobalIniFiles.EventTimer(Sender: TObject);
begin
  SetPost;
end;

class function TGlobalIniFiles.GetIniFiles: TGlobalIniFiles;
begin
  Result := GlobalIniFiles;
end;

{ TSetting }

class function TSetting.GetEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

class function TSetting.ReadString(const AParam: String; ADefault: String): String;
begin
  Result := GlobalIniFiles.ReadString(AParam,ADefault);
end;

class function TSetting.ReadInteger(const AParam: String;
  Default: Integer): Integer;
begin
  Result := StrToIntDef(TSetting.ReadString(AParam),Default);
end;

class function TSetting.ReadBool(const AParam: String;
  Default: Boolean): Boolean;
var
  xS: String;
begin
  xS := TSetting.ReadString(AParam);
  if SameStr(xS,'false') or SameStr(xS,'0') then
    Result := false
  else
  begin
    if SameStr(xS,'true') or SameStr(xS,'1') then
      Result := true
    else
      Result := Default;
  end;
end;

class function TSetting.ReadDate(const AParam: String;
  Default: TDateTime): TDateTime;
begin
  Result := StrToDateDef(TSetting.ReadString(AParam),Default);
end;

class function TSetting.ReadDateTime(const AParam: String;
  Default: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(TSetting.ReadString(AParam),Default);
end;

class function TSetting.ReadFloat(const AParam: String;
  Default: Double): Double;
begin
  Result := StrToFloatDef(TSetting.ReadString(AParam),Default);
end;

class function TSetting.ReadTime(const AParam: String;
  Default: TDateTime): TDateTime;
begin
  Result := StrToTimeDef(TSetting.ReadString(AParam),Default);
end;

class procedure TSetting.SetPost;
begin
  GlobalIniFiles.SetPost;
end;

class procedure TSetting.WriteString(const AParam: String; AValue: String);
begin
  GlobalIniFiles.WriteString(AParam,AValue);
end;

class procedure TSetting.WriteInteger(const AParam: String; AValue: Integer);
begin
  TSetting.WriteString(AParam,IntToStr(AValue));
end;

class procedure TSetting.WriteBool(const AParam: String; Value: Boolean);
begin
  TSetting.WriteInteger(AParam,Integer(Value));
end;

class procedure TSetting.WriteDate(const AParam: String; Value: TDateTime);
begin
  TSetting.WriteString(AParam,DateToStr(Value));
end;

class procedure TSetting.WriteDateTime(const AParam: String; Value: TDateTime);
begin
  TSetting.WriteString(AParam,DateTimeToStr(Value));
end;

class procedure TSetting.WriteFloat(const AParam: String; Value: Double);
begin
  TSetting.WriteString(AParam,FloatToStr(Value))
end;

class procedure TSetting.WriteTime(const AParam: String; Value: TDateTime);
begin
  TSetting.WriteString(AParam,TimeToStr(Value));
end;

initialization
  GlobalIniFiles := TGlobalIniFiles.Create;

finalization
  FreeAndNil(GlobalIniFiles);

end.
