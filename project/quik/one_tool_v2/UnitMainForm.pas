unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  UnitBarsFrame,
  Lb.SysUtils.Candel, FMX.ListBox, FMX.Objects;

type
  TMainForm = class(TForm)
    ButtonQuikTableGrid: TButton;
    GridPanelLayoutPrice: TGridPanelLayout;
    LayoutQuikBar: TLayout;
    LayoutExpectation: TLayout;
    ButtonStart: TButton;
    Timer: TTimer;
    ButtonSearchStructure: TButton;
    ListBox1: TListBox;
    Text1: TText;
    Text2: TText;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure ButtonQuikTableGridClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonSearchStructureClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    QuikBars: TBarsFrame;
    BookBars: TBarsFrame;
    VectorStructure: TVectorStructure;
    StructureSearch: TStructureSearch;
    procedure StructureSearchOnAddStructurePatern(Sender: TObject);
    procedure StructureSearchOnStopSearchThread(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Quik.ValueTable,
  Quik.Manager.DDE,
  UnitQuikTableGridForm;

const
  FILE_NAME_TEST   = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\source.csv';

  SOURCE_COUNT = 5;
  FUTURE_COUNT = 2;

{ TMainForm }

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
    ButtonStart.Text := 'Стоп'
  else
    ButtonStart.Text := 'Старт';
end;




procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _SelectCandels(ACandels: TCandelList);
  var
    i, iCount: Integer;
    xIndex: Integer;
    xQuikTable: TQuikTable;
    xCandel: TCandel;
    xCandels: TCandelList;
  begin
    ACandels.Clear;
    xIndex := QuikManagerTable.IndexOfTable('cnadel_srz2');
    if xIndex >= 0 then
    begin
      xCandels := TCandelList.Create;
      try
        // --------------------------------------------
        // Запрашиваем количество свечей
        xQuikTable := QuikManagerTable.Tables[xIndex];
        xQuikTable.Last;

        var xSourceCount := 0;
        while not xQuikTable.BOF do
        begin
          xCandel.Open  := xQuikTable.AsDouble('open');
          xCandel.High  := xQuikTable.AsDouble('high');
          xCandel.Low   := xQuikTable.AsDouble('low');
          xCandel.Close := xQuikTable.AsDouble('close');
          xCandel.Vol   := xQuikTable.AsDouble('volume');
          xCandels.Add(xCandel);
          Inc(xSourceCount);
          if xSourceCount >= SOURCE_COUNT then
            Break;
          xQuikTable.Prior;
        end;
        // ---------------------------------------------
        // Реверсируем  записи
        iCount := xCandels.Count;
        if iCount > 0 then
          for i := iCount - 1 downto 0 do
          begin
            xCandel := xCandels[i];
            ACandels.Add(xCandel);
          end;
        // ---------------------------------------------
      finally
        FreeAndNil(xCandels);
      end;
    end;
  end;

  procedure _SelectValueStructure(const AStructure: TValueStructure);
  var
    iCount: Integer;
    xCandel, xFutureCandel: TCandel;
  begin
    AStructure.Clear;
    _SelectCandels(AStructure.SourceVectors);

    iCount := AStructure.SourceVectors.Count;
    if iCount > 0 then
    begin
      xCandel := AStructure.SourceVectors[iCount - 1];
      for var i := 0 to FUTURE_COUNT - 1 do
      begin
        xFutureCandel.Open  := xCandel.Close;
        xFutureCandel.High  := xCandel.Close;
        xFutureCandel.Low   := xCandel.Close;
        xFutureCandel.Close := xCandel.Close;
        AStructure.FutureVectors.Add(xFutureCandel);
      end;
    end;
  end;

var
  xValueStructure: TValueStructure;
begin
  xValueStructure := TValueStructure.Create;
  try
    _SelectValueStructure(xValueStructure);
    VectorStructure.Transform(xValueStructure);
    // ----------------------------------------
    // Включить поиск
    StructureSearch.FileName := FILE_NAME_TEST;
    StructureSearch.SourceCount := SOURCE_COUNT;
    StructureSearch.FutureCount := FUTURE_COUNT;
    Text1.Text := 'Старт потока';
    QuikBars.SetShowStructure(VectorStructure);
    if StructureSearch.GetVectorStructure(VectorStructure) then
      Text2.Text := '';
  finally
    FreeAndNil(xValueStructure);
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  QuikBars := TBarsFrame.Create(nil);
  QuikBars.Parent := LayoutQuikBar;
  QuikBars.Align := TAlignLayout.Client;

  BookBars := TBarsFrame.Create(nil);
  BookBars.Parent := LayoutExpectation;
  BookBars.Align := TAlignLayout.Client;

  VectorStructure := TVectorStructure.Create;


  StructureSearch := TStructureSearch.Create;
  StructureSearch.OnAddStructurePatern := StructureSearchOnAddStructurePatern;
  StructureSearch.OnStopSearchThread := StructureSearchOnStopSearchThread;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(VectorStructure);
  FreeAndNil(BookBars);
  FreeAndNil(QuikBars);

  FreeAndNil(StructureSearch);
  inherited;
end;

procedure TMainForm.ListBox1Click(Sender: TObject);

  procedure _SetStructurePaternResult(const AIndex: Integer);
  var
    xStructurePatern: TStructurePatern;
  begin
    if AIndex >= 0 then
    begin
      xStructurePatern := StructureSearch.StructurePaterns[AIndex];
      BookBars.SetShowStructure(xStructurePatern.Structure);
    end;
  end;

var
  xIndex: Integer;
begin
  xIndex := ListBox1.ItemIndex;
  if xIndex >= 0 then
    _SetStructurePaternResult(xIndex);
end;

procedure TMainForm.ButtonQuikTableGridClick(Sender: TObject);
begin
  TQuikTableGridForm.ShowQuikTableGrid;
end;

procedure TMainForm.ButtonSearchStructureClick(Sender: TObject);
begin
  // Запускаем поиск
end;

procedure TMainForm.StructureSearchOnAddStructurePatern(Sender: TObject);
var
  xIndex: Integer;
  xS: String;
  i, iCount: Integer;
  xStructurePatern: TStructurePatern;
begin
  ListBox1.BeginUpdate;
  try
    ListBox1.Items.Clear;
    iCount := StructureSearch.StructurePaterns.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xStructurePatern := StructureSearch.StructurePaterns[i];
        xS := 'patern ' +
          xStructurePatern.LengthPrice.ToString + '; ' +
          xStructurePatern.LengthVol.ToString + ';';
        ListBox1.Items.Add(xS);
      end;
  finally
    ListBox1.EndUpdate;
  end;

  //xIndex := StructureSearch.ResultTrandPatern;
  //if xIndex >= 0 then
  //  SetStructurePaternResult(xIndex);
end;

procedure TMainForm.StructureSearchOnStopSearchThread(Sender: TObject);
begin
  Text2.Text := 'Стоп потока';
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if Assigned(StructureSearch) then
  begin

    ProgressBar1.Value := StructureSearch.ProgressReading;
  end;
end;

end.
