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
  FMX.TabControl,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo.Types,
  FMX.StdCtrls,
  FMX.Memo, FMX.Layouts;

type
  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabItemSource: TTabItem;
    ButtonLoadSources: TButton;
    SourceLayout: TLayout;
    TabItemBlocks: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoadSourcesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.ChartsFrame,
  Lb.SourcesFrame,
  Lb.Candel.Source,
  Lb.Candel.Vector,
  Lb.Candel.DB;

var
  localCandels: TSourceCandel;
  localSourcesFrame: TSourcesFrame;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TSourcesFrame.SetFreeAndNil;
  FreeAndNil(localCandels)
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Генерирование формы свечей';
  localCandels := TSourceCandel.Create;
  localSourcesFrame := TSourcesFrame.GetCreateFrame(SourceLayout);
end;

procedure TMainForm.ButtonLoadSourcesClick(Sender: TObject);
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0)) + 'export.csv';
  localCandels.SetLoadFile(xPath);
  localSourcesFrame.SetSource(localCandels);
end;

end.
