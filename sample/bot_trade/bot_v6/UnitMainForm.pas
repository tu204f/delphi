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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ListBox,
  FMX.Layouts,
  Lb.Module.SysUtils;

type
  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    StatusText: TText;
    TimerUpData: TTimer;
    LayoutMenu: TLayout;
    ButtonOpen: TButton;
    ButtonSS: TButton;
    LayoutModule: TLayout;
    TrackBar: TTrackBar;
    TextSpeed: TText;
    procedure ButtonOpenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerUpDataTimer(Sender: TObject);
    procedure ButtonSSClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FModule: IModule;
    procedure SetStop;
  protected
    procedure SetCreateFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Setting;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  TrackBar.Value := TSetting.ReadInteger(SPEED_TIMER_UPDATA,1);
  Self.Caption := 'Тестирование торговых стратегий';
  StatusText.Text := TSetting.ReadString(CONFIG_FILE_NAME,'');
  SetCreateFrame;
end;

procedure TMainForm.ButtonSSClick(Sender: TObject);
begin
  case TimerUpData.Enabled of
    True: SetStop;
    False: begin
      if Assigned(FModule) then
        if FModule.Start then
        begin
          TimerUpData.Enabled := not TimerUpData.Enabled;
          ButtonSS.Text := 'Стоп';
        end;
    end;
  end;
end;

procedure TMainForm.SetCreateFrame;
begin
  FModule := GetCreateClassFrameByName('CHARTS_CANDELS',LayoutModule);
  if Assigned(FModule) then
    Self.Caption := FModule.Caption;
end;

procedure TMainForm.SetStop;
begin
  TimerUpData.Enabled := not TimerUpData.Enabled;
  ButtonSS.Text := 'Старт';
  if Assigned(FModule) then
    FModule.Stop;
end;

procedure TMainForm.TimerUpDataTimer(Sender: TObject);
begin
  try
    if Assigned(FModule) then
    begin
      if not FModule.UpData then
        SetStop;
    end;
  except
    SetStop;
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);

  function _IntervalTimer(const AValue: Integer): Integer;
  begin
    Result := AValue * AValue;
  end;

var
  xInterval: Integer;
begin
  xInterval := Trunc(TrackBar.Value);
  TSetting.WriteInteger(SPEED_TIMER_UPDATA,xInterval);
  TimerUpData.Interval := _IntervalTimer(xInterval); // xInterval * 100;
  TextSpeed.Text := 'Скорость обновление: ' + IntToStr(TimerUpData.Interval) + ' мс.';
end;

procedure TMainForm.ButtonOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    TSetting.WriteString(CONFIG_FILE_NAME,OpenDialog.FileName);
    StatusText.Text := OpenDialog.FileName;
  end;
end;

end.
