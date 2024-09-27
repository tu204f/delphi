unit UnitPlatformSettingFrame;

interface

{$i platform.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Edit,
  UnitSettingBybitFrame,
  UnitSettingQuikFrame,
  Lb.SysUtils,
  FMX.ListBox,
  FMX.TabControl;

type
  TPlatformSettingFrame = class(TFrame)
    Rectangle: TRectangle;
    Layout: TLayout;
    TabControl: TTabControl;
    TabItemBybit: TTabItem;
    TabItemQuik: TTabItem;
  private
    FSettingBybit: TSettingBybitFrame;
    FSettingQuik : TSettingQuikFrame;
    FMainApp: IMainApp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSetting;
    procedure SaveSetting;
    procedure SetHeaderCaption;
    property MainApp: IMainApp write FMainApp;
  end;

implementation

uses
  Lb.ApplicationVersion;

{$R *.fmx}

{ TSettingFrame }

constructor TPlatformSettingFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSettingBybit := TSettingBybitFrame.Create(nil);
  FSettingBybit.Parent := TabItemBybit;
  FSettingBybit.Align := TAlignLayout.Client;

  FSettingQuik  := TSettingQuikFrame.Create(nil);
  FSettingQuik.Parent := TabItemQuik;
  FSettingQuik.Align := TAlignLayout.Client;
end;

destructor TPlatformSettingFrame.Destroy;
begin

  inherited;
end;

procedure TPlatformSettingFrame.LoadSetting;
begin
  ParamApplication.Load;
  FSettingBybit.Load;
  FSettingQuik.Load;
  SetHeaderCaption;
end;

procedure TPlatformSettingFrame.SetHeaderCaption;
var
  xS: String;
begin
  xS := 'xBot - ' + GetStrToTypePlatform(ParamApplication.TypePlatform) + ' [' + GetApplicationVersion + '] ';
{$IFDEF DEBUG}
  xS := xS + ' debug';
{$ENDIF}
  FMainApp.SetHeaderCaption(xS);
  case ParamApplication.TypePlatform of
    TTypePlatform.tpBybit: TabControl.ActiveTab := TabItemBybit;
    TTypePlatform.tpQuik: TabControl.ActiveTab := TabItemQuik;
  end;
end;


procedure TPlatformSettingFrame.SaveSetting;
begin
  case ParamApplication.TypePlatform of
    tpBybit: FSettingBybit.Save;
    tpQuik: FSettingQuik.Save;
  end;
end;

end.
