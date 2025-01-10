{ pkgs }: {
  PAGER = "less";
  CLICLOLOR = 1;
  EDITOR = "nvim";
  JAVA_HOME = "${pkgs.jre}";
}
