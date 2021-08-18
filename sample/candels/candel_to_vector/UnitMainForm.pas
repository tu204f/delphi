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
  FMX.Dialogs, FMX.TabControl, FMX.Layouts;

type
  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabItemSource: TTabItem;
    TabItemVector: TTabItem;
    LayoutSource: TLayout;
    LayoutVector: TLayout;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetInitializationSource;
    procedure SetFinalizationSource;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  UnitSourceDataFrame;

var
  localCandelFrame: TSourceDataFrame = nil;
  localVectorFrame: TSourceDataFrame = nil;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Преобразование:';
  Self.SetInitializationSource;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.SetFinalizationSource;
end;

procedure TMainForm.SetInitializationSource;
begin
  // -----------------------------------------------
  localCandelFrame := TSourceDataFrame.Create(nil);
  localCandelFrame.Parent := LayoutSource;
  localCandelFrame.Align := TAlignLayout.Client;
  // -----------------------------------------------
  localVectorFrame := TSourceDataFrame.Create(nil);
  localVectorFrame.Parent := LayoutVector;
  localVectorFrame.Align := TAlignLayout.Client;
end;

procedure TMainForm.SetFinalizationSource;
begin
  FreeAndNil(localVectorFrame);
  FreeAndNil(localCandelFrame);
end;

end.
