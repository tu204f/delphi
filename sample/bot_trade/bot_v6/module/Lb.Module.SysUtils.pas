unit Lb.Module.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Layouts,
  FMX.Types,
  FMX.Forms;

const
  CONFIG_FILE_NAME = 'config.file_name';
  SPEED_TIMER_UPDATA = 'config.speed_interval';

type
  TClassFrame = class of TFrame;

const
  IID_MODULE: TGUID = '{C3A5D484-F5E9-4A55-A113-D0D357D8EE50}';

type
  IModule = interface
  ['{C3A5D484-F5E9-4A55-A113-D0D357D8EE50}']
  {private}
    ///<summary>Наименование фрема</summary>
    function GetCaption: WideString;
  {public} {передоваемы событие на frame}
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
  {public}
    property Caption: WideString read GetCaption;
  end;

function GetCreateClassFrame(const AIndex: Integer; AParent: TLayout): IModule;
function GetCreateClassFrameByName(const AName: String; AParent: TLayout): IModule;

///<summary>Возвращает номер зарегистрированного класс</summary>
function RegistrationFrameIndexOf(AName: String): Integer;
///<summary>Зарегистрированный класс (Frame)</summary>
procedure RegistrationFrame(AName: String; AClassFrame: TClassFrame);

implementation

uses
  UnitDefaultFrame;

type
  TRegistrationFrame = record
    Name: String;
    ClassFrame: TClassFrame;
  end;

  TClassFrameList = TList<TRegistrationFrame>;

var
  localRegistrationFrame: TClassFrameList = nil;

function GetRegistrationFrame: TClassFrameList;
begin
  if not Assigned(localRegistrationFrame) then
    localRegistrationFrame := TClassFrameList.Create;
  Result := localRegistrationFrame;
end;

function RegistrationFrameIndexOf(AName: String): Integer;
var
  i, iCount: Integer;
  xRegistrationFrame: TRegistrationFrame;
begin
  Result := -1;
  iCount := GetRegistrationFrame.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xRegistrationFrame := GetRegistrationFrame.Items[i];
      if SameText(AName,xRegistrationFrame.Name) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

procedure RegistrationFrame(AName: String; AClassFrame: TClassFrame);
var
  xRegistrationFrame: TRegistrationFrame;
begin
  xRegistrationFrame.Name := AName;
  xRegistrationFrame.ClassFrame := AClassFrame;
  GetRegistrationFrame.Add(xRegistrationFrame);
end;

function GetCreateClassFrameByName(const AName: String; AParent: TLayout): IModule;
var
  xIndex: Integer;
begin
  xIndex := RegistrationFrameIndexOf(AName);
  Result := GetCreateClassFrame(xIndex,AParent);
end;

function GetCreateClassFrame(const AIndex: Integer; AParent: TLayout): IModule;
var
  xFrame: TFrame;
  xClassFrame: TClassFrame;
  xModule: IModule;
begin
  if (AIndex >= 0) and  (AIndex < GetRegistrationFrame.Count) then
  begin
    xClassFrame := GetRegistrationFrame.Items[AIndex].ClassFrame;
    xFrame := xClassFrame.Create(nil);
  end
  else
    xFrame := TDefaultFrame.Create(nil);

  if Assigned(xFrame) then
  begin
    xFrame.Parent := AParent;
    xFrame.Align := TAlignLayout.Client;
    xFrame.GetInterface(IID_MODULE,xModule);
    if Assigned(xModule) then
      Result := xModule
    else
      raise Exception.Create('Error Message: Модуль не определен');
  end
  else
    raise Exception.Create('Error Message: Frame - нет проблема');
end;

initialization

finalization
  FreeAndNil(localRegistrationFrame);

end.
