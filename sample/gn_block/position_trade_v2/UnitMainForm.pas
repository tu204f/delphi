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
  FMX.Memo.Types,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.StdCtrls,
  Lb.ReadPrice,
  Lb.Pattern,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  UnitBarsFrame;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    Timer: TTimer;
    ButtonStop: TButton;
    ProgressBar: TProgressBar;
    Text1: TText;
    Text2: TText;
    Memo: TMemo;
    GridLayout: TGridPanelLayout;
    CandelsLayout: TLayout;
    ParentsLayout: TLayout;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  protected
    CandelIndex: Integer;
    CandelsSource: TCandelsSource;
    ParrentCandels: TParrentCandelList;
    Candels: TCandelList;
    procedure DoStart;
    procedure DoStop;
  protected
    FCandelsFrame: TBarsFrame;
    FParentsFrame: TBarsFrame;
    procedure SetInitCandelsFrame;
    procedure SetInitParentsFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddCol(AGrid: TStringGrid; AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AGrid);
    xCol.Header := AHeader;
    xCol.Parent := AGrid;
  end;

begin
  inherited;

  SetInitCandelsFrame;
  SetInitParentsFrame;

  CandelsSource := TCandelsSource.Create;
  Candels := TCandelList.Create;
  ParrentCandels := TParrentCandelList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FParentsFrame);
  FreeAndNil(FCandelsFrame);

  FreeAndNil(ParrentCandels);
  FreeAndNil(Candels);
  FreeAndNil(CandelsSource);
  inherited;
end;

procedure TMainForm.SetInitCandelsFrame;
begin
  FCandelsFrame := TBarsFrame.Create(nil);
  FCandelsFrame.Parent := CandelsLayout;
  FCandelsFrame.Align := TAlignLayout.Client;
end;

procedure TMainForm.SetInitParentsFrame;
begin
  FParentsFrame := TBarsFrame.Create(nil);
  FParentsFrame.Parent := ParentsLayout;
  FParentsFrame.Align := TAlignLayout.Client;
end;

procedure TMainForm.DoStart;
var
  xFileName: String;
begin
  if not Timer.Enabled then
  begin
    ParrentCandels.Clear;

    CandelIndex := 0;
    xFileName := ExtractFilePath(ParamStr(0)) + 'data\';

    //xFileName := xFileName + 'GAZP_240401_240630.csv';
    //xFileName := xFileName + 'GAZP_240401_240630.csv';
    //xFileName := xFileName + 'GAZP_240501_240731.csv';
    //xFileName := xFileName + 'GAZP_240601_240804.csv';
    xFileName := xFileName + 'GAZP_240701_240917.csv';
    //xFileName := xFileName + 'SPFB.GAZR-9.24_240701_240919.csv';

    CandelsSource.LoadFromFile(xFileName);
    CandelsSource.Delete(0);
    Timer.Enabled := True;
  end;
end;

procedure TMainForm.DoStop;
begin
  if Timer.Enabled then
  begin
    Timer.Enabled := False;
  end;
end;


procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  DoStart;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  DoStop;
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _ShowParent;
  var
    i, iCount: Integer;
    xParrentCandel: TParrentCandel;
  begin
    iCount := ParrentCandels.Count;
    if iCount > 0 then
    begin
      Memo.Lines.Add('** парент ------------------------------');
      for i := 0 to iCount - 1 do
      begin
        xParrentCandel := ParrentCandels.Items[i];
        Memo.Lines.Add(xParrentCandel.ToStr);
      end;
    end;
  end;

var
  xCandel: TCandel;
begin
  Memo.Lines.Clear;
  if CandelsSource.Count > CandelIndex then
  begin
    xCandel := CandelsSource.GetSelectedCandel(CandelIndex);
    if Candels.Count < 50 then
    begin
      Candels.Add(xCandel);
      Memo.Lines.Add('** Заполняем массив свячей');
    end
    else
    begin
      Candels.Delete(0);
      Candels.Add(xCandel);
      ToCandelParrent(
        Candels,
        ParrentCandels,
        False
       );
      FCandelsFrame.SetShowStructure(Candels);
      FParentsFrame.SetShowParants(ParrentCandels);
      //DoStop;
    end;
    _ShowParent;
    Inc(CandelIndex);
  end
  else
    DoStop;
end;

end.
