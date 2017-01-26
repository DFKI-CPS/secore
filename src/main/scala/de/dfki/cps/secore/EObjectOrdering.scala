package de.dfki.cps.secore

import org.eclipse.emf.ecore.{EAttribute, EObject, EReference}
import scala.collection.JavaConverters._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  *
class EObjectOrdering extends Ordering[EObject] {
  def compare(x: EObject, y: EObject): Int = {
    val xCls = x.eClass()
    val yCls = y.eClass()
    if (xCls != yCls) {
      Ordering.String.compare(xCls.getName, yCls.getName)
    } else {
      val features = xCls.getEAllStructuralFeatures.asScala
      features.filter(f =>
        !f.isDerived &&
        !f.isTransient &&
        !f.isVolatile
      )
      val attributes = features.collect {
        case attr: EAttribute => attr
      }
      val refs = features.collect {
        case ref: EReference => ref
      }
      attributes.find(_.isID).fold {

      } { id =>
        val fac = id.getEAttributeType.getEPackage.getEFactoryInstance
        val xid = fac.convertToString(id.getEAttributeType, x.eGet(id))
        val yid = fac.convertToString(id.getEAttributeType, y.eGet(id))
        Ordering.String.compare(xid,yid)
      }
    }
  }
}
*/