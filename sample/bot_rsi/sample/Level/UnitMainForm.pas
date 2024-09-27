unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, Lb.Level;

type
  TMainForm = class(TForm)
    Timer: TTimer;
    ButtonStart: TButton;
    EditValueRSI: TEdit;
    Memo: TMemo;
    EditStatusLevel: TEdit;
    EditUpDown: TEdit;
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    procedure SetCaptionButton;
    procedure EventOnIntersectionLevel(Sender: TObject);
  public
    ValueRSI: Double;
    Increment: Double;
    Status: Integer;
    procedure SetLog(const S: String);
  public
    Level: TOneEventLevel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Level := TOneEventLevel.Create;

  Level.IsRepeat := False;
  Level.RepeatCount := 3;
  Level.WorkLevel := tlDownUp;
  //Level.WorkLevel := tlUpDown;

  Level.OnIntersectionLevel := EventOnIntersectionLevel;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Level);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  SetCaptionButton;
end;

procedure TMainForm.SetCaptionButton;
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
  begin
    Memo.Lines.Clear;
    Status := 1;
    Increment := 1;
    ValueRSI := 50;
    ButtonStart.Text := 'Стоп';

    Level.Value := 50;
  end
  else
    ButtonStart.Text := 'Старт';
end;

procedure TMainForm.SetLog(const S: String);
begin
  Memo.Lines.Add(S);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  try
    case Status of
      1: ValueRSI := ValueRSI + Increment;
      2: ValueRSI := ValueRSI - Increment;
    end;

    if ValueRSI >= 60 then
    begin
      Status := 2;
      Memo.Lines.Clear;
    end;

    if ValueRSI <= 40 then
    begin
      Status := 1;
      Memo.Lines.Clear;
    end;

    Level.SetUpDate(ValueRSI);

    SetLog(
      ValueRSI.ToString + ' :: ' +
      StatusLevelToStr(Level.StatusLevel) + ' :: ' +
      IntersectionLevelToStr(Level.IntersectionLevel)
    );


    EditStatusLevel.Text := 'Level: ' + StatusLevelToStr(Level.StatusLevel);
    EditUpDown.Text := IntersectionLevelToStr(Level.IntersectionLevel);
    EditValueRSI.Text := ValueRSI.ToString;
  except
    SetCaptionButton;
  end;
end;

procedure TMainForm.EventOnIntersectionLevel(Sender: TObject);
var
  xS: String;
begin
  Memo.Lines.Add('TMainForm.EventOnIntersectionLevel:');
  xS := 'Level: ' + StatusLevelToStr(Level.StatusLevel) + ' ' +
        IntersectionLevelToStr(Level.IntersectionLevel);
  Memo.Lines.Add(xS);

  Level.WorkLevel := TIntersectionLevel.tlDownUp;
end;

end.
