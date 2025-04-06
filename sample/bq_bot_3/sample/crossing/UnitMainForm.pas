unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  Lb.Crossing, FMX.Edit;

type
  TForm4 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Timer1: TTimer;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    CountCrodding: Integer;
    ValueParam: Double;
    Crossing: TCrossing;
    StatusCrossing: Integer;
    procedure EventCrossing(ASender: TObject; AValue: Double; ATypeCrossing: TTypeCrossing);
    procedure EventCrossingValue(ASender: TObject; AValue: Double; ATypeCrossing: TTypeCrossing);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.FormCreate(Sender: TObject);
begin
  StatusCrossing := 0;
  Crossing := TCrossing.Create;
  Crossing.OnCrossing := EventCrossing;
  Crossing.OnCrossingValue := EventCrossingValue;
end;

procedure TForm4.EventCrossing(ASender: TObject; AValue: Double; ATypeCrossing: TTypeCrossing);
begin
//  case ATypeCrossing of
//    tcNull: ListBox1.Items.Add('EventCrossing.Null ' + AValue.ToString);
//    tcHigh: ListBox1.Items.Add('EventCrossing.High ' + AValue.ToString);
//    tcLow : ListBox1.Items.Add('EventCrossing.Low ' + AValue.ToString);
//  end;

  CountCrodding := CountCrodding - 1;
  if CountCrodding = 0 then
    case StatusCrossing of
      0: StatusCrossing := 1;
      1: StatusCrossing := 0;
    end;

end;

procedure TForm4.EventCrossingValue(ASender: TObject; AValue: Double; ATypeCrossing: TTypeCrossing);
begin
  CountCrodding := 5;
  case ATypeCrossing of
    tcNull: ListBox1.Items.Add('EventCrossingValue.Null ' + AValue.ToString);
    tcHigh: ListBox1.Items.Add('EventCrossingValue.High ' + AValue.ToString);
    tcLow : ListBox1.Items.Add('EventCrossingValue.Low ' + AValue.ToString);
  end;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Crossing);
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  ValueParam := 10 + Random(5)/2;
  Crossing.SetNewValue(ValueParam,3,0.5);
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TForm4.Timer1Timer(Sender: TObject);
begin
  Edit1.Text := ValueParam.ToString;
  case StatusCrossing of
    0: ValueParam := ValueParam + 0.1;
    1: ValueParam := ValueParam - 0.1;
  end;
  Crossing.SetValue(ValueParam);

  Edit2.Text := Crossing.ValueHigh.ToString;
  Edit3.Text := Crossing.ValueLow.ToString;
end;

end.
