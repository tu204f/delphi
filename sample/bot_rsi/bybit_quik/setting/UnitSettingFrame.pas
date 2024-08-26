unit UnitSettingFrame;

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
  TSettingFrame = class(TFrame)
    Rectangle: TRectangle;
    ButtonApply: TButton;
    ButtonClose: TButton;
    Layout: TLayout;
    ComboBoxPlatform: TComboBox;
    LayoutButton: TLayout;
    Text1: TText;
    TabControl: TTabControl;
    TabItemBybit: TTabItem;
    TabItemQuik: TTabItem;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ComboBoxPlatformChange(Sender: TObject);
  private
    FSettingBybit: TSettingBybitFrame;
    FSettingQuik : TSettingQuikFrame;
    FMainApp: IMainApp;
    procedure SetHeaderCaption;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSetting;
    procedure SaveSetting;
    property MainApp: IMainApp write FMainApp;
  end;

implementation

uses
  Lb.ApplicationVersion;

{$R *.fmx}

{ TSettingFrame }

procedure TSettingFrame.ComboBoxPlatformChange(Sender: TObject);
var
  xIndex: Integer;
begin
  xIndex := ComboBoxPlatform.ItemIndex;
  case xIndex of
    0: begin
      TabControl.ActiveTab := TabItemBybit;
      ParamApplication.TypePlatform := TTypePlatform.tpBybit;
    end;
    1: begin
      TabControl.ActiveTab := TabItemQuik;
      ParamApplication.TypePlatform := TTypePlatform.tpQuik;
    end;
  end;
  SetHeaderCaption;
end;

constructor TSettingFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSettingBybit := TSettingBybitFrame.Create(nil);
  FSettingBybit.Parent := TabItemBybit;
  FSettingBybit.Align := TAlignLayout.Client;

  FSettingQuik  := TSettingQuikFrame.Create(nil);
  FSettingQuik.Parent := TabItemQuik;
  FSettingQuik.Align := TAlignLayout.Client;
end;

destructor TSettingFrame.Destroy;
begin

  inherited;
end;

procedure TSettingFrame.LoadSetting;
begin
  ParamApplication.Load;
  FSettingBybit.Load;
  FSettingQuik.Load;
  ComboBoxPlatform.ItemIndex := Integer(ParamApplication.TypePlatform);
  SetHeaderCaption;
end;

procedure TSettingFrame.SetHeaderCaption;
var
  xS: String;
begin
  xS := 'xBot - ' + GetStrToTypePlatform(ParamApplication.TypePlatform) + ' [' + GetApplicationVersion + '] ';
{$IFDEF DEBUG}
  xS := xS + ' debug';
{$ENDIF}
  FMainApp.SetHeaderCaption(xS);
end;


procedure TSettingFrame.SaveSetting;
begin
  case ParamApplication.TypePlatform of
    tpBybit: FSettingBybit.Save;
    tpQuik: FSettingQuik.Save;
  end;
  ParamApplication.Save;
end;


procedure TSettingFrame.ButtonApplyClick(Sender: TObject);
begin
  SaveSetting;
end;

procedure TSettingFrame.ButtonCloseClick(Sender: TObject);
begin
  FMainApp.EventCloseTabControl;
end;

end.
