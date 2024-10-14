unit UnitSettingFrame;

interface

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
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.TabControl,
  Lb.SysUtils,
  UnitPlatformSettingFrame;

type
  TSettingFrame = class(TFrame)
    Rectangle: TRectangle;
    LayoutButton: TLayout;
    ButtonApply: TButton;
    ButtonClose: TButton;
    Layout: TLayout;
    TabControl: TTabControl;
    TabItemPlatformSetting: TTabItem;
    LayoutPlatform: TLayout;
    ComboBoxPlatform: TComboBox;
    Text1: TText;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure ComboBoxPlatformChange(Sender: TObject);
  private
    FMainApp: IMainApp;
    procedure SetMainApp(const Value: IMainApp);
  protected
    FPlatformSetting: TPlatformSettingFrame;
    procedure SetIsLayoutPlatform;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSetting;
    property MainApp: IMainApp write SetMainApp;
  end;

implementation

{$R *.fmx}

{ TSettingFrame }

procedure TSettingFrame.ComboBoxPlatformChange(Sender: TObject);
var
  xIndex: Integer;
begin
  xIndex := ComboBoxPlatform.ItemIndex;
  case xIndex of
    0: ParamPlatform.TypePlatform := TTypePlatform.tpBybit;
    1: ParamPlatform.TypePlatform := TTypePlatform.tpQuik;
  end;
  FPlatformSetting.SetHeaderCaption;
end;

constructor TSettingFrame.Create(AOwner: TComponent);
begin
  inherited;
  FPlatformSetting := TPlatformSettingFrame.Create(nil);
  FPlatformSetting.Parent := TabItemPlatformSetting;
  FPlatformSetting.Align := TAlignLayout.Client;
  SetIsLayoutPlatform;
end;

destructor TSettingFrame.Destroy;
begin
  FreeAndNil(FPlatformSetting);
  inherited;
end;


procedure TSettingFrame.LoadSetting;
begin
  case ParamPlatform.TypePlatform of
    TTypePlatform.tpBybit: ComboBoxPlatform.ItemIndex := 0;
    TTypePlatform.tpQuik: ComboBoxPlatform.ItemIndex := 1;
  end;
  FPlatformSetting.LoadSetting;
end;

procedure TSettingFrame.SetIsLayoutPlatform;
begin
  LayoutPlatform.Visible := (TabControl.ActiveTab = TabItemPlatformSetting);
end;

procedure TSettingFrame.SetMainApp(const Value: IMainApp);
begin
  FPlatformSetting.MainApp := Value;
  FMainApp := Value;
end;

procedure TSettingFrame.TabControlChange(Sender: TObject);
begin
  SetIsLayoutPlatform;
end;

procedure TSettingFrame.ButtonApplyClick(Sender: TObject);
begin
  FPlatformSetting.SaveSetting;
  ParamPlatform.Save;
end;

procedure TSettingFrame.ButtonCloseClick(Sender: TObject);
begin
  FMainApp.EventCloseTabControl;
end;

end.
