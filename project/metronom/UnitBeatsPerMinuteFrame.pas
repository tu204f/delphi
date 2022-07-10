unit UnitBeatsPerMinuteFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation;

type
  TBeatsPerMinuteFrame = class(TFrame)
    Layout: TLayout;
    GridPanelLayout: TGridPanelLayout;
    Circle: TCircle;
    LayoutTools: TLayout;
    Text: TText;
    ButtonPlus: TButton;
    ButtonMinus: TButton;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
  private
    { Private declarations }
    FOnBeatPer: TNotifyEvent;
    FValueBPM: Integer;
    procedure SetValueBPM(const Value: Integer);
  protected
    procedure DoBeatPer;
    property ValueBPM: Integer read FValueBPM write SetValueBPM;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property OnBeatPer: TNotifyEvent  write FOnBeatPer;
  end;

implementation

{$R *.fmx}

{ TBeatsPerMinuteFrame }

constructor TBeatsPerMinuteFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ValueBPM := 60;
end;

destructor TBeatsPerMinuteFrame.Destroy;
begin

  inherited;
end;

procedure TBeatsPerMinuteFrame.SetValueBPM(const Value: Integer);
var
  xL: Double;
begin
  FValueBPM := Value;
  xL := (60/FValueBPM) * 1000;
  if xL = 0 then
  begin
    xL := 1000;
    FValueBPM := 60;
  end;
  Timer.Interval := Trunc(xL);
  Text.Text := IntToStr(FValueBPM);
end;

procedure TBeatsPerMinuteFrame.ButtonMinusClick(Sender: TObject);
begin
  ValueBPM := ValueBPM - 1;
end;

procedure TBeatsPerMinuteFrame.ButtonPlusClick(Sender: TObject);
begin
  ValueBPM := ValueBPM + 1;
end;

procedure TBeatsPerMinuteFrame.DoBeatPer;
begin
  if Assigned(FOnBeatPer) then
    FOnBeatPer(Self);
end;

procedure TBeatsPerMinuteFrame.TimerTimer(Sender: TObject);
begin
  DoBeatPer;
end;

procedure TBeatsPerMinuteFrame.Start;
begin
  Timer.Enabled := True;
  //DoBeatPer;
end;

procedure TBeatsPerMinuteFrame.Stop;
begin
  Timer.Enabled := False;
end;

end.
