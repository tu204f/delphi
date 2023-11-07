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
  UnitLogFrame,
  FMX.Layouts,
  FMX.ListBox;

type
  TMainForm = class(TForm, ILogger, IStockMarket)
    TabControl: TTabControl;
    TabItemTrade: TTabItem;
    TabItemLog: TTabItem;
    Layout2: TLayout;
    Layout1: TLayout;
    ButtonStart: TButton;
    ButtonStop: TButton;
    procedure FormShow(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  protected
    MainFrame: TMainFrame;
    LogFrame: TLogFrame;
    procedure DoBegin;
    procedure DoEnd;
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
  MainFrame.StockMarket := Self;
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

procedure TMainForm.DoBegin;
begin
  MainFrame.DoBegin;
  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;
end;

procedure TMainForm.DoEnd;
begin
  MainFrame.DoEnd;
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
  TabControl.ActiveTab := TabItemTrade;
end;

procedure TMainForm.Log(S: String);
begin
  LogFrame.Log(S);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  //
end;

end.
