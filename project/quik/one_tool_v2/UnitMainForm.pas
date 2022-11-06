unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  UnitBarsFrame,
  Lb.SysUtils.Candel;

type
  TMainForm = class(TForm)
    ButtonQuikTableGrid: TButton;
    GridPanelLayoutPrice: TGridPanelLayout;
    LayoutQuikBar: TLayout;
    LayoutExpectation: TLayout;
    ButtonStart: TButton;
    Timer: TTimer;
    ButtonSearchStructure: TButton;
    procedure ButtonQuikTableGridClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonSearchStructureClick(Sender: TObject);
  private
    QuikBars: TBarsFrame;
    BookBars: TBarsFrame;
    VectorStructure: TVectorStructure;
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
  SOURCE_COUNT = 20;
  FUTURE_COUNT = 5;

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
    QuikBars.SetShowStructure(VectorStructure);
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
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(VectorStructure);
  FreeAndNil(BookBars);
  FreeAndNil(QuikBars);
  inherited;
end;

procedure TMainForm.ButtonQuikTableGridClick(Sender: TObject);
begin
  TQuikTableGridForm.ShowQuikTableGrid;
end;

procedure TMainForm.ButtonSearchStructureClick(Sender: TObject);
begin
  // Запускаем поиск
end;

end.
