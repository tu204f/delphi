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
  UnitTableFrame;

  // тест

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
  private

  protected
    procedure InitFrame;
    procedure EventCloseTabControl;
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
  Self.Caption := 'xBot - bybit [' + GetApplicationVersion + '] ';
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

  TabControl.ActiveTab := TabItemMain;
end;

procedure TMainForm.EventCloseTabControl;
begin
  TabControl.ActiveTab := TabItemMain;
end;

procedure TMainForm.ButtonSettingClick(Sender: TObject);
begin
  SettingFrame.LoadSetting;
  TabControl.ActiveTab := TabItemSetting;
end;

procedure TMainForm.ButtonStartStopClick(Sender: TObject);
begin
  // Запускам или остановливаем торговую стратегию
  if MainClientFrame.StatusFrame.IsActive then
  begin
    ButtonStartStop.Text := 'Старт';
    MainClientFrame.StatusFrame.Stop;
  end
  else
  begin
    ButtonStartStop.Text := 'Стоп';
    MainClientFrame.StatusFrame.Start;
  end;
end;

procedure TMainForm.ButtonTableClick(Sender: TObject);
begin
  // Показывать результат работы программы
  TabControl.ActiveTab := TabItemTable;
end;

end.
