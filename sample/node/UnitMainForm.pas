unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SimpleGraph, Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    SimpleGraph: TSimpleGraph;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);

  function _GetCreateNode(const ANodeX, ANodeY: Integer): TEllipticNode;
  var
    xNodex: TEllipticNode;
  begin
    xNodex := TEllipticNode.Create(SimpleGraph);
    with xNodex do
    begin
      SetBounds(
        ANodeX + Owner.Width div 2 - 10,
        ANodeY + Owner.Height div 2 - 10, 20, 20);
      NodeOptions := NodeOptions - [gnoResizable];
      Brush.Color := clRed;
    end;
  end;


  function _GetCreateLink(): TGraphLink;
  begin

  end;

var
  xNode1, xNode2: TEllipticNode;
begin
  xNode1 := _GetCreateNode(Random(100),Random(100));
  xNode2 := _GetCreateNode(Random(100),Random(100));
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  //
end;

end.
