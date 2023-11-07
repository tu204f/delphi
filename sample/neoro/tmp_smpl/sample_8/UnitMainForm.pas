unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Generics.Collections, FMX.TabControl,
  Lb.SysUtils,
  Lb.Sources,
  UnitMainFrame,
  UnitLogFrame, FMX.Layouts, FMX.ListBox;

type
  TMainForm = class(TForm, ILogger)
    TabControl: TTabControl;
    TabItemTrade: TTabItem;
    TabItemLog: TTabItem;
    ComboBox1: TComboBox;
    Layout1: TLayout;
    Layout2: TLayout;
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  protected
    MainFrame: TMainFrame;
    LogFrame: TLogFrame;
    procedure LaodSources;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(S: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }



constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Caption := 'Тестирование системы';
  // -------------------------------------------
  MainFrame := TMainFrame.Create(nil);
  MainFrame.Parent := TabItemTrade;
  MainFrame.Align  := TAlignLayout.Client;
  MainFrame.Logger := Self;
  // -------------------------------------------
  LogFrame := TLogFrame.Create(nil);
  LogFrame.Parent := TabItemLog;
  LogFrame.Align  := TAlignLayout.Client;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(LogFrame);
  FreeAndNil(MainFrame);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  TabControl.ActiveTab := TabItemTrade;
  LaodSources;
end;

procedure TMainForm.Log(S: String);
begin
  LogFrame.Log(S);
end;

procedure TMainForm.LaodSources;

  function _Load(const AFileName: String): TSource;
  var
    xSource: TSource;
    xFN: String;
  begin
    xSource := TSource.Create;

    xFN := 'source\' + AFileName;
    xSource.LoadFromFile(xFN);
    xSource.Delete(0);

    Result := xSource;
  end;

var
  xS: String;
begin
  for var i := 1 to 10 do
  begin
    xS := 'SPFB.CNY_' + i.ToString + '.csv';
    ComboBox1.Items.AddObject(xS,_Load(xS));
  end;
  ComboBox1.ItemIndex := 0;
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
var
  xIndex: Integer;
  xSource: TSource;
begin
  xIndex := ComboBox1.ItemIndex;
  if xIndex >= 0 then
  begin
    xSource := TSource(ComboBox1.Items.Objects[xIndex]);
    MainFrame.CandelSource := xSource;
  end;
end;

end.
