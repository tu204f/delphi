unit UnitMainForm;

interface

// 1. Нужно найти все условия - что бы отфильровать

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
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Lb.SysUtils.Candel, FMX.StdCtrls, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.Objects;

type
  TStatusOperation = (soStart, soStop);

  TMainForm = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    Timer: TTimer;
    StrGrid: TStringGrid;
    StringColumnTime: TStringColumn;
    StringColumnPrice: TStringColumn;
    StringColumnVol: TStringColumn;
    Text: TText;
    TradeGrid: TStringGrid;
    StrColumnTime: TStringColumn;
    StrColumnPrice: TStringColumn;
    StrColumnQuantity: TStringColumn;
    StrColumnBuySell: TStringColumn;
    StrColumnProfit: TStringColumn;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FPrice, FVol: Double;
    FTikets: TTiketList;
    FMemoryTikets: TMemoryTikets;
    procedure SetTiketBuffer(const ATiket: TTiket);
    procedure SetTiketListBox;
    procedure SetStatusOperation(const AStatus: TStatusOperation);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

const
  FILE_NAME_TIKE = 'data\sber\SPFB.SBRF-12.22_221111_221111.csv';

procedure SetWeightedDecision(const ATikets: TTiketList; var APrice, AVol: Double);
var
  xTiket: TTiket;
  xSumValue: Double;
begin
  APrice := 0;
  AVol := 0;
  xSumValue := 0;
  for xTiket in ATikets do
  begin
    xSumValue := xSumValue + xTiket.Value;
    AVol := AVol + xTiket.Vol;
  end;
  if AVol > 0 then
    APrice := xSumValue/AVol;
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMemoryTikets := TMemoryTikets.Create;
  FTikets := TTiketList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FTikets);
  FreeAndNil(FMemoryTikets);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetStatusOperation(TStatusOperation.soStop);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  SetStatusOperation(TStatusOperation.soStart);
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  SetStatusOperation(TStatusOperation.soStop);
end;

procedure TMainForm.SetStatusOperation(const AStatus: TStatusOperation);
begin
  case AStatus of
    soStart: begin
      ButtonStart.Enabled := False;
      ButtonStop.Enabled := True;
      FMemoryTikets.FileName := FILE_NAME_TIKE;
      FMemoryTikets.First;
      Timer.Enabled := True;
    end;
    soStop: begin
      ButtonStart.Enabled := True;
      ButtonStop.Enabled := False;
      Timer.Enabled := False;
    end;
  end;
end;

procedure TMainForm.SetTiketBuffer(const ATiket: TTiket);
const
  TIKETS_COUNT = 20;

  procedure _AddTiketBuffer(const ATiket: TTiket);
  begin
    if FTikets.Count >= TIKETS_COUNT then
      FTikets.Delete(0);
    FTikets.Add(ATiket);
  end;

var
  xTiket: TTiket;
  xIndex: Integer;
begin
  if FTikets.Count > 0 then
  begin
    xIndex := FTikets.Count - 1;
    xTiket := FTikets.Items[xIndex];
    if xTiket.Price = ATiket.Price then
    begin
      xTiket.Time := ATiket.Time;
      xTiket.Vol := xTiket.Vol + ATiket.Vol;
      FTikets[xIndex] := xTiket;
    end
    else
      _AddTiketBuffer(ATiket);
  end
  else
    _AddTiketBuffer(ATiket);
end;

procedure TMainForm.SetTiketListBox;
var
  xTiket: TTiket;
begin
  SetWeightedDecision(FTikets, FPrice, FVol);
  FPrice := Round(FPrice);
  Text.Text := 'Price: ' + FloatToStr(FPrice) + ' :: ' + FloatToStr(FVol);

  StrGrid.BeginUpdate;
  try
    var iCount := FTikets.Count;
    StrGrid.RowCount := iCount;
    if FTikets.Count > 0 then
    begin
      var xIndex := iCount - 1;
      for var i := 0 to iCount - 1 do
      begin
        xTiket := FTikets[xIndex - i];
        with StrGrid do
        begin
          Cells[0,i] := TimeToStr(xTiket.Time);
          Cells[1,i] := xTiket.Price.ToString;
          Cells[2,i] := xTiket.Vol.ToString;
        end;
      end;
    end;
  finally
    StrGrid.EndUpdate;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xTiket: TTiket;
begin
  try
    if FMemoryTikets.EOF then
    begin
      SetStatusOperation(TStatusOperation.soStop);
    end
    else
    begin
      xTiket := FMemoryTikets.Tiket;

      SetTiketBuffer(xTiket);
      SetTiketListBox;

      FMemoryTikets.Next;
    end;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      SetStatusOperation(TStatusOperation.soStop);
    end;
  end;
end;

end.
