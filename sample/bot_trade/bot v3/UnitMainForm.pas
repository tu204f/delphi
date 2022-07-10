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
  FMX.Layouts,
  FMX.Objects,
  Lb.Candel.SysUtils,
  Lb.Candel.Source,
  Lb.ChartsFrame, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListBox;

type
  TMainForm = class(TForm)
    ButtonLoad: TButton;
    ButtonRandomSelected: TButton;
    ButtonSearchVector: TButton;
    StatusText: TText;
    LayoutChart: TLayout;
    Timer: TTimer;
    ListBox: TListBox;
    Button1: TButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonRandomSelectedClick(Sender: TObject);
    procedure ButtonSearchVectorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ListBoxChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

    SourceCandel: TSourceCandel;
    ChartsFrame: TCandelsCharFrame;
    SearchCandels: TCandelList;
    SearchVectors: TCandelList;

    procedure BlackBoxOnBeginComing(Sender: TObject);
    procedure BlackBoxOnEndComing(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Vecrot.Candel,
  Lb.Black.Box;

const
  VECTOR_COUNT = 20;
  COMING_COUNT = 3;

var
  localBlackBox: TBlackBox = nil;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SourceCandel := TSourceCandel.Create;

  SearchCandels:= TCandelList.Create;
  SearchVectors:= TCandelList.Create;

  ChartsFrame := TCandelsCharFrame.Create(nil);
  ChartsFrame.Parent := LayoutChart;
  ChartsFrame.Align := TAlignLayout.Client;

  localBlackBox := TBlackBox.Create;
  localBlackBox.OnBeginComing := BlackBoxOnBeginComing;
  localBlackBox.OnEndComing := BlackBoxOnEndComing;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(localBlackBox);

  FreeAndNil(SearchVectors);
  FreeAndNil(SearchCandels);

  FreeAndNil(ChartsFrame);
  FreeAndNil(SourceCandel);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Программа прогнозирование событий:';
end;


procedure TMainForm.ButtonLoadClick(Sender: TObject);
begin
  //SourceCandel.SetLoadFile('d:\work\git\delphi\sample\bot_trade\bot v2\bin\rts\rts_one_minut.csv'); //rts_5_minut.csv
  //SourceCandel.SetLoadFile('d:\work\git\delphi\sample\bot_trade\bot v2\bin\rts\19.csv');
  SourceCandel.SetLoadFile('d:\work\git\delphi\sample\bot_trade\bot v2\bin\rts\SPFB.RTS_100101_220622.csv');
  StatusText.Text := 'Загрузка: ' + SourceCandel.Candels.Count.ToString;

  localBlackBox.Vectors.VectorCount := VECTOR_COUNT;
  localBlackBox.Vectors.ComingCount := COMING_COUNT;
  localBlackBox.Vectors.SetLoadCandels(SourceCandel.Candels);
end;

procedure TMainForm.ButtonRandomSelectedClick(Sender: TObject);

  procedure SetSelectedVector(const AIndex, ACount: Integer; AVector: TCandelList);
  begin
    if not Assigned(AVector) then
      Exit;
    AVector.Clear;
    for var i := 0 to ACount - 1 do
    begin
      var xInd := AIndex + i;
      if xInd >= SourceCandel.Candels.Count then
      begin
        AVector.Clear;
        Break;
      end;
      var xCandel := SourceCandel.Candels[xInd];
      AVector.Add(xCandel);
    end;
  end;


begin
  // Выбираем случайный набор данных
  var xIndex := Random(SourceCandel.Candels.Count);
  SetSelectedVector(xIndex,VECTOR_COUNT,SearchCandels);
end;

procedure TMainForm.ButtonSearchVectorClick(Sender: TObject);
begin
  // Запускаем посик подходящего варианта
  localBlackBox.SearchCandel(SearchCandels);
end;

procedure TMainForm.BlackBoxOnBeginComing(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TMainForm.BlackBoxOnEndComing(Sender: TObject);
begin
  Timer.Enabled := False;
  TimerTimer(nil);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  ListBox.Items.Clear;
  for var xComing in localBlackBox.Comings do
    ListBox.Items.Add(xComing.ToString);
end;

procedure TMainForm.ListBoxChange(Sender: TObject);
var
  xCandels: TCandelList;
begin
  var xIndex := ListBox.ItemIndex;
  if xIndex >= 0 then
  begin
    xCandels := TCandelList.Create;
    try
      localBlackBox.ComingCandel(xIndex,xCandels);
      ChartsFrame.ShowCandels(SearchCandels,xCandels);
    finally
      FreeAndNil(xCandels);
    end;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  SearchCandels.Clear;
  for var i := 0 to VECTOR_COUNT - 1 do
  begin
    var xInd := (SourceCandel.Candels.Count - VECTOR_COUNT) + i;
    var xCandel := SourceCandel.Candels[xInd];
    SearchCandels.Add(xCandel);
  end;
end;


end.
