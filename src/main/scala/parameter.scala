package com
import swing._
import event._
import java.awt.{Color, Graphics2D, Point, geom}
abstract class parameter{
    def getComponent() : Component
}
class Range(call: ((Double)=>Unit),
            start: Double = 0.0,
            max: Double = 1.0, min: Double = 0.0,
            step: Double = 0.01
                ) extends parameter{
    def getComponent() : Component = {
        import javax.swing.SpinnerNumberModel
        import javax.swing.JSpinner
        import javax.swing.event.ChangeListener
        import javax.swing.event.ChangeEvent
        val model   = new SpinnerNumberModel(start, min, max, step)
        val spinner = new JSpinner(model)
        spinner.addChangeListener( new ChangeListener {
            def stateChanged(event: ChangeEvent){
                call(model.getNumber().doubleValue());
            }
        } )
        Component.wrap(spinner)
    }
}
abstract class choiceParameter extends parameter{
    //default widget implementation is dropdown list
    def getChoices() :List[String]
    def getChosen() :String
    def setChosen(opt :String) :Unit
    def getComponent() : Component = new ComboBox(getChoices()) {
        listenTo(selection)
        reactions += {
            case SelectionChanged(_) => setChosen(selection.item)
        }
        peer.setSelectedItem(getChosen())
    }
}
trait buttons extends choiceParameter {
    val orientation: Orientation.Value = Orientation.Vertical
    override
    def getComponent() : Component = new BoxPanel(orientation) {
        val buttons = getChoices().map(n => new Button(n))
        buttons.foreach(listenTo(_))
        contents ++= buttons
        reactions += {
            case ButtonClicked(x) => setChosen(x.text)
        }
    }
}
class choiceList(val choices: List[(String, ()=>Any)]) extends choiceParameter {
    val getChoices = choices.map(x => x._1)
    var getChosen  = getChoices head
    def setChosen(opt :String) :Unit = {
        val func = choices.find(x => x._1 == opt)
        if(func == None) throw new Exception //error
        else {
            getChosen = func.get._1
            func.get._2()
        }
    }
}
class caseParam[T](initial: T, ops: List[T], action: ((T) => Unit)) extends choiceParameter{
    val getChoices = ops.map(x => x.toString)
    var getChosen = initial.toString
    def setChosen(c: String) :Unit = {
        val choice = ops.find(x => x.toString == c)
        if(choice == None) throw new Exception
        else {
            getChosen = c
            action(choice.get)
        }
    }
}
class positionWidget(monitors: List[Monitor]) extends Panel {
    val bgColor    = new Color(242,233,225)
    val guideColor = new Color(193,186,180)
    val fgColor    = new Color(203,232,107)
    val bdrColor   = new Color(28,20,13)
    val textColor  = new Color(28,20,13)
    val scale : Double = 12.0;
    val lfont = new Font("Sans Serif", java.awt.Font.PLAIN, 16*scale.toInt)
    val mGlyphs = monitors.map(m => new mGlyph(m)).reverse
    def width  = size.getWidth()  toInt
    def height = size.getHeight() toInt
    def refresh() { repaint() }
    preferredSize = {
        val len = mGlyphs.foldLeft(0){
            case (s,m) => s + Math.max(m.width,m.height)
        } / scale.toInt
        new Dimension(len,len)
    }
    //helper to draw multiple, center alligned strings
    def cntTxt(x: Integer, y: Integer, lines: List[String], g: Graphics2D){
        if(lines != Nil) {
            val size = g.getFontMetrics.getStringBounds(lines.head, g)
            val strWidth  = size.getWidth().toInt
            val strHeight = size.getHeight().toInt
            g.drawString(lines.head, x - strWidth/2, y)
            cntTxt(x, y+strHeight, lines.tail, g)
        }
    }
    class mGlyph(val monitor: Monitor){
        def xpos = monitor.position.x
        def ypos = monitor.position.y
        def width = monitor.resolution.x
        def height = monitor.resolution.y
        def horzLines: List[Integer] = List(ypos, ypos+height/2, ypos+height)
        def vertLines: List[Integer] = List(xpos, xpos+width /2, xpos+width )

        def hit(p: Point) = {
            val dx = p.x - xpos
            val dy = p.y - ypos
            (dx > 0) && (dy > 0) && (dx < width) && (dy < height)
        }
        def applyDelta(d: Point, horz: List[Integer], vert: List[Integer]){
            val pos = monitor.position
            val hL = horzLines
            val vL = vertLines
            val h = horz.exists(x => hL.exists(y => Math.abs(x-y) < scale ))
            val v = vert.exists(x => vL.exists(y => Math.abs(x-y) < scale ))
            val THRES = 2*scale
            val deltaX = if(!v || (Math.abs(d.x) > THRES)) d.x else 0
            val deltaY = if(!h || (Math.abs(d.y) > THRES)) d.y else 0
            monitor.position = new Res(
                Math.max(0, pos.x + deltaX),
                Math.max(0, pos.y + deltaY) )
        }
        def paint(g: Graphics2D){
            g.setPaint(fgColor)
            g.fillRect(xpos, ypos, width, height)
            g.setColor(bdrColor)
            g.drawRect(xpos, ypos, width, height)
            cntTxt( xpos+width/2, ypos + height/2,
                     List(monitor.name, monitor.position.toString),
                     g )
        }
    }
    override def paintComponent(g: Graphics2D) = {
        import java.awt.RenderingHints
        val rh = new RenderingHints(
             RenderingHints.KEY_TEXT_ANTIALIASING,
             RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        g.setRenderingHints(rh);

        g.setColor(bgColor)
        g.fillRect(0,0,width,height)

        g.scale(1.0/scale, 1.0/scale);

        g.setColor(guideColor)
        val horz = mGlyphs.flatMap(m => m.horzLines)
        val vert = mGlyphs.flatMap(m => m.vertLines)
        for(h <- horz) g.drawLine(0, h, width*scale.toInt , h)
        for(v <- vert) g.drawLine(v, 0, v, height*scale.toInt)

        g.setFont(lfont)
        g.setStroke(new java.awt.BasicStroke(scale.toInt))
        mGlyphs.foreach(m => m.paint(g))
    }
    //mouse section
    listenTo(mouse.clicks, mouse.moves)
    private[this] var clicked: mGlyph = null
    private[this] var lastPos: Point  = new Point(0,0)
    private[this] var horzGuides: List[Integer] = Nil
    private[this] var vertGuides: List[Integer] = Nil
    def relPos(p: Point) = new Point(p.x*scale.toInt, p.y*scale.toInt)
    def delta(p: Point): Point = {
        val delta = new Point(p.x - lastPos.x, p.y - lastPos.y)
        lastPos = p
        return delta
    }
    reactions += {
        case e: MousePressed => {
            val press = relPos(e.point)
            val found = mGlyphs.find( _.hit(press) )
            if(found != None){
                clicked = found.get
                val others = mGlyphs.filter(x => x != clicked)
                horzGuides = others.flatMap(m => m.horzLines)
                vertGuides = others.flatMap(m => m.vertLines)
            }
            delta(press)
            refresh()
        }
        case e: MouseDragged => {
            val press = relPos(e.point)
            val d = delta(press)
            if(clicked != null) {
                clicked.applyDelta(d, horzGuides, vertGuides)
            }
            refresh()
        }
        case e: MouseReleased => {
            clicked = null
            horzGuides = Nil
            vertGuides = Nil
            refresh()
        }
    }
}
