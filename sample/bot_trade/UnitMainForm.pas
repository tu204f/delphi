unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Lb.Candel.SysUtils,
  Lb.Candel.Source, Vcl.Grids;

type
  TMainForm = class(TForm)
    Timer: TTimer;
    ButtonLoad: TButton;
    MemoLog: TMemo;
    ButtonStart: TButton;
    ButtonStream: TButton;
    CandelGrid: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStreamClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    procedure SetCandelGrid(const ARowID: Integer; ACandel: TCandel);
    procedure Start;
    procedure Stop;
    procedure Next;
  protected
    procedure SetLog(S: String);
  public
    CandelStream: TCandelStream;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CandelStream := TCandelStream.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CandelStream);
end;

procedure TMainForm.FormShow(Sender: TObject);

  procedure _GridTitle(const AGrid: TStringGrid);
  begin
    with AGrid.Rows[0] do
    begin
      Clear;
      Add('Date');
      Add('Time');
      Add('Open');
      Add('High');
      Add('Low');
      Add('Close');
      Add('Value');
    end;
  end;

begin
  Self.Caption := 'Работы объекта';
  _GridTitle(CandelGrid);
end;

procedure TMainForm.SetLog(S: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:nn:ss.zzz',Time) + ' || ' + S;
  MemoLog.Lines.Add(xS);
end;

procedure TMainForm.Start;
begin
//  if SourceCandel.Candels.Count > 0 then
//  begin
//    FIndexCandel := 0;
//    ButtonStart.Caption := 'Стоп';
//    Timer.Enabled := True;
//  end;
end;

procedure TMainForm.Stop;
begin
  ButtonStart.Caption := 'Старт';
  Timer.Enabled := False;
end;

procedure TMainForm.SetCandelGrid(const ARowID: Integer; ACandel: TCandel);
begin
  with CandelGrid.Rows[ARowID] do
  begin
    Clear;
    Add(DateToStr(ACandel.Date));
    Add(TimeToStr(ACandel.Time));
    Add(FloatToStr(ACandel.Open));
    Add(FloatToStr(ACandel.High));
    Add(FloatToStr(ACandel.Low));
    Add(FloatToStr(ACandel.Close));
    Add(FloatToStr(ACandel.Vol));
  end;
end;

procedure TMainForm.Next;
const
  SIZE_COUNT_CANDEL = 10;
var
  i: Integer;
begin
  CandelStream.First;
  i := 1;
  while i <= SIZE_COUNT_CANDEL do
  begin
    SetCandelGrid(i,CandelStream.Candel);
    CandelStream.Next;
    Inc(i);
  end;
  CandelGrid.RowCount := SIZE_COUNT_CANDEL + 1;
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
begin
  // Сразу загружаем определеное количетсов свечей
  // И наченаем иметации, торговых операци
  // а взадачу ИИ - в ходит на учиться торговать
  // Определяем путь к файлу
  CandelStream.FileName := 'd:\work\git\delphi\sample\data\sber\data.csv';
  // Читаем первый блок данных и выводим в таблицу
  Next;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  // Старт - ратации
  if Timer.Enabled then
    Stop
  else
    Start;
end;

procedure TMainForm.ButtonStreamClick(Sender: TObject);
var
  xS: String;
  xL: INteger;
  xCandelStream: TCandelStream;
begin
  xCandelStream := TCandelStream.Create;
  try
    xL := 100;
    xCandelStream.FileName := 'd:\work\git\delphi\sample\data\sber\data.csv';
    xS := xCandelStream.First.ToString;
    while not xCandelStream.EOF do
    begin
      xS := xCandelStream.Next.ToString;
      MemoLog.Lines.Add(xS);
      Dec(xL);
      if xL <= 0 then
        Break;
    end;
  finally
    FreeAndNil(xCandelStream);
  end;
end;

end.
