unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  Lb.ReadPrice.Tiket,
  FMX.Objects,
  Lb.Position.Trade, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMXTee.Engine,
  FMXTee.Series, FMXTee.Procs, FMXTee.Chart;

type
  TMainForm = class(TForm)
    ButtonLoad: TButton;
    RectangleTiketCount: TRectangle;
    TextTiketCount: TText;
    RectangleTiket: TRectangle;
    TextTiket: TText;
    ButtonStart: TButton;
    Timer: TTimer;
    Memo: TMemo;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    Index: Integer;
    function UpDateTiket: Boolean;
    procedure SetTitketLast(AIndex: Integer; ATiket: TTiket);
  protected
    procedure SetLog(S: String);
  public
    { Public declarations }
    TiketsSource: TTiketsSource;
    Trade: TTrade;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

var
  MainForm: TMainForm;

procedure SetLog(S: String);

implementation

{$R *.fmx}

procedure SetLog(S: String);
begin
  MainForm.SetLog(S);
end;


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  TiketsSource := TTiketsSource.Create;
  Trade := TTrade.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Trade);
  FreeAndNil(TiketsSource);
  inherited;
end;


procedure TMainForm.Start;
begin
  ButtonStart.Text := 'Стоп';
  Timer.Enabled := True;
  Index := 0;
end;

procedure TMainForm.Stop;
begin
  ButtonStart.Text := 'Старт';
  Timer.Enabled := False;
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
begin
  var xFileName := ExtractFilePath(ParamStr(0)) + 'data\';
  xFileName := xFileName + 'GAZP_240920_240920.csv';
  TiketsSource.LoadFromFile(xFileName);
  TiketsSource.Delete(0);
  TextTiketCount.Text := 'Количество сделок: ' + TiketsSource.Count.ToString;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
    ButtonStart.Text := 'Старт';
    Stop;
  end
  else
  begin
    ButtonStart.Text := 'Стоп';
    Start;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  try
    for var i := 1 to 10 do
      if not UpDateTiket then
      begin
        Stop;
        Break;
      end;
  except
    Stop;
  end;
end;

function TMainForm.UpDateTiket: Boolean;
var
  xT: TTiket;
begin
  if TiketsSource.Count >= Index then
  begin
    Result := True;
    xT := TiketsSource.Tikets[Index];
    SetTitketLast(Index,xT);
    Inc(Index);
  end
  else
    Result := False;
end;

procedure TMainForm.SetLog(S: String);
begin
  Memo.Lines.Add(S);
  if Memo.Lines.Count > 50 then
    Memo.Lines.Delete(0);
end;

procedure TMainForm.SetTitketLast(AIndex: Integer; ATiket: TTiket);

  function GetNumberToStr(const AIndex: Integer): String;
  var
    xL: Integer;
    xS: String;
  begin
    xS := AIndex.ToString;
    xL := xS.Length;
    case xL of
      1: Result := '0000' + xS;
      2: Result := '000' + xS;
      3: Result := '00' + xS;
      4: Result := '0' + xS;
    else
      Result := xS;
    end;
  end;

begin
  TextTiket.Text := GetNumberToStr(Index) + ' ' + ATiket.ToStr;
  Trade.SetTitketLast(ATiket);
end;


end.
