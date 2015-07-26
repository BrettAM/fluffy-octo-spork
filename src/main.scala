package com
import com._
import swing._
import event._
    /*
        disable dpms and screensaver for a certain amount of time
        Add a way to save and store different monitor configurations
        add a way to save monitor state to x config file
        add a way to lock together brightnesses
    */
object main extends SimpleSwingApplication{
    //used to stop the garbage collector from stealing callbacks
    val monitors = Monitor.get().filter(x=>x.connected)

    val res = for(res <- monitors.head.resList) yield {
        res.toString -> (() => println("Set to "+res))//setRes(res))
    }

    def top = new MainFrame {
        title = "XRW"
        contents = new BoxPanel(Orientation.Vertical) {
            val positionMover = new positionWidget(monitors)
            val refreshDisplay : ()=>Unit = () => {
                positionMover.refresh()
            }
            contents += positionMover
            for(monitor <- monitors){
                println(monitor)
                def setRes(x: Res): Unit = {
                    monitor.resolution = x
                    refreshDisplay()
                }
                def setRot(x: Rotation): Unit = {
                    monitor.rotation = x
                    refreshDisplay()
                }
                def setRef(x: Reflection): Unit = {
                    monitor.reflection = x
                    refreshDisplay()
                }
                def setBrightness(d: Double): Unit = {
                    monitor.brightness = d
                    refreshDisplay()
                }
                val res = for(res <- monitor.resList) yield {
                    res.toString -> (() => setRes(res))
                }
                val parameters :List[parameter] = List(
                    new choiceList(res),
                    new caseParam(monitor.rotation, Rotation.all, setRot(_:Rotation)),
                    new caseParam(monitor.reflection, Reflection.all, setRef(_:Reflection)),
                    new Range(setBrightness(_), start = 1.0, step = 0.1)
                    )
                contents += new BoxPanel(Orientation.Horizontal) {
                    contents ++= parameters.map(x => x.getComponent)
                }
            }
            contents += new BoxPanel(Orientation.Horizontal) {
                val applyButton = new Button("Apply"){
                    maximumSize = new Dimension(8000,8000)
                }
                listenTo(applyButton)
                reactions += {
                    case ButtonClicked(_) => {
                        for(monitor <- monitors) monitor.publish()
                        refreshDisplay()
                    }
                }
                contents += applyButton
            }
        }
    }
}

