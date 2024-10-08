unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.TabControl,
  Lb.SysUtils,
  Lb.ApplicationVersion,
  UnitMainClientFrame,
  UnitSettingFrame,
  UnitTableFrame, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm, IMainApp)
    LayoutMenu: TLayout;
    ButtonSetting: TButton;
    ButtonTable: TButton;
    LayoutClient: TLayout;
    TabControl: TTabControl;
    TabItemSetting: TTabItem;
    TabItemMain: TTabItem;
    ButtonStartStop: TButton;
    TabItemTable: TTabItem;
    ButtonTactics: TButton;
    procedure ButtonSettingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ButtonTableClick(Sender: TObject);
    procedure ButtonTacticsClick(Sender: TObject);
  private

  protected
    procedure InitFrame;
    procedure EventCloseTabControl;
    procedure SetHeaderCaption(const ACaption: String);
  public
    SettingFrame: TSettingFrame;
    MainClientFrame: TMainClientFrame;
    TableFrame: TTableFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Self.InitFrame;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(MainClientFrame);
  FreeAndNil(SettingFrame);
  FreeAndNil(TableFrame);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption :=
    'xBot - ' + GetStrToTypePlatform(ParamApplication.TypePlatform) +
    ' [' + GetApplicationVersion + '] ';
{$IFDEF DEBUG}
  Self.Caption := Self.Caption + ' debug';
{$ENDIF}
end;

procedure TMainForm.InitFrame;

  procedure _InitSetting;
  begin
    SettingFrame := TSettingFrame.Create(nil);
    SettingFrame.Parent := TabItemSetting;
    SettingFrame.Align := TAlignLayout.Client;
    SettingFrame.MainApp := Self;
  end;

  procedure _InitMainClientFrame;
  begin
    MainClientFrame := TMainClientFrame.Create(nil);
    MainClientFrame.Parent := TabItemMain;
    MainClientFrame.Align := TAlignLayout.Client;
  end;

  procedure _InitTableFrame;
  begin
    TableFrame := TTableFrame.Create(nil);
    TableFrame.Parent := TabItemTable;
    TableFrame.Align := TAlignLayout.Client;
    TableFrame.MainApp := Self;
  end;

begin
  _InitSetting;
  _InitMainClientFrame;
  _InitTableFrame;
  SettingFrame.LoadSetting;
  TabControl.ActiveTab := TabItemMain;
end;

procedure TMainForm.SetHeaderCaption(const ACaption: String);
begin
  Self.Caption := ACaption;
end;

procedure TMainForm.EventCloseTabControl;
begin
  TabControl.ActiveTab := TabItemMain;
end;

procedure TMainForm.ButtonSettingClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabItemSetting;
end;

procedure TMainForm.ButtonStartStopClick(Sender: TObject);
begin
  // �������� ��� ������������� �������� ���������
  if MainClientFrame.StatusFrame.Status.IsActive then
  begin
    ButtonStartStop.Text := '�����';
    MainClientFrame.StatusFrame.Status.Stop;
  end
  else
  begin
    ButtonStartStop.Text := '����';
    MainClientFrame.StatusFrame.Status.Start;
  end;
end;

procedure TMainForm.ButtonTableClick(Sender: TObject);
begin
  // ���������� ��������� ������ ���������
  TabControl.ActiveTab := TabItemTable;
end;

procedure TMainForm.ButtonTacticsClick(Sender: TObject);
begin
  //
end;

end.
