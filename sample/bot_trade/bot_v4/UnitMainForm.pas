unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Lb.HistoryCandels,
  UnitHistoryCandels,
  UnitCandelsFrame, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    Layout1: TLayout;
    CandelNext: TButton;
    Button2: TButton;
    MemoLog: TMemo;
    ProgressBar1: TProgressBar;
    ButtonStructureNext: TButton;
    procedure FormShow(Sender: TObject);
    procedure CandelNextClick(Sender: TObject);
    procedure ButtonStructureNextClick(Sender: TObject);
  private
    FHistorys: THistoryFrame;
  protected
    property Historys: THistoryFrame read FHistorys;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Candel.SysUtils, Lb.Candel.StructuresFile;

const
  ///<summary>Формирукм воображение</summary>
  FILE_NAME_DATA = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\data.csv';
  ///<summary>На ком тренеруем воображение</summary>
  FILE_NAME_BASE = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\source.csv';

var
  FormationStructures: TFormationStructures = nil;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHistorys := THistoryFrame.Create(nil);
  FHistorys.Parent := Layout1;
  FHistorys.Align := TAlignLayout.Client;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FHistorys);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FHistorys.FileNameData := FILE_NAME_DATA;
  FHistorys.FileNameBase := FILE_NAME_BASE;
  FHistorys.LoadFile;
end;

procedure TMainForm.CandelNextClick(Sender: TObject);
begin
  FHistorys.Next;
end;

procedure TMainForm.ButtonStructureNextClick(Sender: TObject);
begin
  //
end;

end.
