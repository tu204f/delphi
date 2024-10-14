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
    procedure ButtonSettingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ButtonTableClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure StatusOnStart(Sender: TObject);
    procedure StatusOnStop(Sender: TObject);
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

uses
  Lb.Status;

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
    'xBot - ' + GetStrToTypePlatform(ParamPlatform.TypePlatform) +
    ' [' + GetApplicationVersion + '] ';
{$IFDEF DEBUG}
  Self.Caption := Self.Caption + ' debug';
{$ENDIF}
  SetBounds(
    Self.Left,
    Self.Top,
    ParamApplication.Width,
    ParamApplication.Height
  );
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ParamApplication.Height := Self.Height;
  ParamApplication.Width := Self.Width;
  ParamApplication.Save;
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

  Status.OnStart := StatusOnStart;
  Status.OnStop := StatusOnStop;
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

procedure TMainForm.StatusOnStart(Sender: TObject);
begin
  ButtonStartStop.Text := 'Стоп';
end;

procedure TMainForm.StatusOnStop(Sender: TObject);
begin
  ButtonStartStop.Text := 'Старт';
end;

procedure TMainForm.ButtonStartStopClick(Sender: TObject);
begin
  // Запускам или остановливаем торговую стратегию
  if Status.IsActive then
    Status.Stop
  else
    Status.Start;
end;

procedure TMainForm.ButtonTableClick(Sender: TObject);
begin
  // Показывать результат работы программы
  TabControl.ActiveTab := TabItemTable;
end;

end.
