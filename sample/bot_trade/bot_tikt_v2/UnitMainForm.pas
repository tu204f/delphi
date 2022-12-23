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
  Lb.SysUtils.Candel, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Objects,
  UnitLineFrame;

type
  TFormMain = class(TForm)
    ButtonStartAnsStop: TButton;
    Timer: TTimer;
    Layout1: TLayout;
    procedure ButtonStartAnsStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetTitleButtom;
  public
    MemoryTikets: TMemoryTikets;
    LineFrame: TLineFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

const
  FILE_NAME_TIKE     = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\SPFB.SBRF-12.22_221111_221111.csv';


{ TFormMain }

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  MemoryTikets := TMemoryTikets.Create;

  LineFrame := TLineFrame.Create(nil);
  LineFrame.Parent := Layout1;
  LineFrame.Align := TAlignLayout.Client;
end;

destructor TFormMain.Destroy;
begin
  FreeAndNil(LineFrame);
  FreeAndNil(MemoryTikets);
  inherited;
end;


procedure TFormMain.SetTitleButtom;
begin
  if Timer.Enabled then
    ButtonStartAnsStop.Text := 'Стоп'
  else
    ButtonStartAnsStop.Text := 'Стоп';
end;

procedure TFormMain.ButtonStartAnsStopClick(Sender: TObject);
begin
  MemoryTikets.FileName := FILE_NAME_TIKE;
  Timer.Enabled := not Timer.Enabled;
  SetTitleButtom;
  MemoryTikets.First;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
var
  xTiket: TTiket;
begin
  if MemoryTikets.EOF then
  begin
    Timer.Enabled := False;
    SetTitleButtom;
  end
  else
  begin
    xTiket := MemoryTikets.Tiket;
    LineFrame.AddTiket(xTiket.Price,xTiket.Vol);
    MemoryTikets.Next;
  end;
end;

end.
